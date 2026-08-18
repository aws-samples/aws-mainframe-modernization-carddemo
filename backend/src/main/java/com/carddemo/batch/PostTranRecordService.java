package com.carddemo.batch;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

/**
 * CBTRN02C paragraph-level port — validates and posts ONE daily transaction.
 *
 * <p>Called per record, in {@code daily_tran_id} order, from the POSTTRAN
 * step's writer. The validation of record N must see the account/tcatbal
 * updates made by record N-1 (legacy 1500-B-LOOKUP-ACCT re-READs the
 * ACCOUNT-FILE that 2800 just REWROTE — posted amounts feed the overlimit
 * check of later records in the same run), which is why this logic cannot sit
 * in a chunk-buffered ItemProcessor: Spring Batch processes a whole chunk
 * before writing it, so an ItemProcessor would validate against stale
 * balances. Executing validate-then-post per item inside the writer preserves
 * the exact legacy read-your-writes sequencing.</p>
 *
 * <p>Validation (1500-VALIDATE-TRAN) — <b>last-wins, not short-circuit</b>
 * (CBTRN02C.cbl:403-421):</p>
 * <ul>
 *   <li>100 card not in xref (1500-A) — the only check that skips the rest;</li>
 *   <li>101 account record not found (1500-B INVALID KEY);</li>
 *   <li>otherwise BOTH run unconditionally:
 *       102 if {@code credit_limit < curr_cyc_credit - curr_cyc_debit + amt},
 *       then 103 if {@code expiration_date < orig_ts(1:10)} — 103 OVERWRITES
 *       102 when both hold.</li>
 * </ul>
 *
 * <p>Posting (2000-POST-TRANSACTION):</p>
 * <ul>
 *   <li>2700: upsert transaction_category_balances (+amt; 2700-A creates the
 *       row with balance = amt when missing);</li>
 *   <li>2800: {@code curr_bal += amt}; {@code amt >= 0 → curr_cyc_credit += amt}
 *       else {@code curr_cyc_debit += amt} (negative amounts drive the debit
 *       bucket negative — legacy behavior, preserved);</li>
 *   <li>2900: insert into transactions keeping {@code TRAN-ID = DALYTRAN-ID},
 *       orig_ts copied, {@code proc_ts = now}.</li>
 * </ul>
 *
 * <p>Reason 109 (2800 REWRITE INVALID KEY, line 556): legacy MOVEs 109 into
 * WS-VALIDATION-FAIL-REASON but the main loop never re-checks it after 2000 —
 * a dead store; the record still lands in TRANSACT and no DALYREJS record is
 * written. Here the path is unreachable anyway (1500-B read the account in
 * the same DB transaction), but if the UPDATE ever hits 0 rows we both write
 * the 109 reject row (per plan §2.2, for observability — deliberate, documented
 * divergence from the legacy silent drop) and still insert the transaction
 * (mirroring 2900 running unconditionally).</p>
 */
@Component
public class PostTranRecordService {

    private static final Logger log = LoggerFactory.getLogger(PostTranRecordService.class);

    public enum Outcome { POSTED, REJECTED }

    private final JdbcTemplate jdbc;

    public PostTranRecordService(JdbcTemplate jdbc) {
        this.jdbc = jdbc;
    }

    /** Validate + post/reject one daily transaction. Never throws for data reasons. */
    public Outcome process(DailyTxn t, long jobRunId) {
        // ---- 1500-A-LOOKUP-XREF (xref dissolved into cards.account_id) ------
        List<Long> acctIds = jdbc.queryForList(
                "SELECT account_id FROM cards WHERE card_number = ?", Long.class, t.cardNumber());
        if (acctIds.isEmpty()) {
            reject(t, jobRunId, 100, "INVALID CARD NUMBER FOUND");
            return Outcome.REJECTED;
        }
        long accountId = acctIds.get(0);

        // ---- 1500-B-LOOKUP-ACCT ---------------------------------------------
        List<AcctRow> accts = jdbc.query("""
                        SELECT credit_limit, curr_cyc_credit, curr_cyc_debit, expiration_date
                        FROM accounts WHERE account_id = ?
                        """,
                (rs, i) -> new AcctRow(
                        rs.getBigDecimal("credit_limit"),
                        rs.getBigDecimal("curr_cyc_credit"),
                        rs.getBigDecimal("curr_cyc_debit"),
                        rs.getObject("expiration_date", LocalDate.class)),
                accountId);
        if (accts.isEmpty()) {
            reject(t, jobRunId, 101, "ACCOUNT RECORD NOT FOUND");
            return Outcome.REJECTED;
        }
        AcctRow acct = accts.get(0);

        // Both checks run; LAST WINS (103 overwrites 102) — CBTRN02C.cbl:403-421.
        int reason = 0;
        String desc = null;
        BigDecimal tempBal = acct.cycCredit().subtract(acct.cycDebit()).add(t.amount());
        if (acct.creditLimit() != null && acct.creditLimit().compareTo(tempBal) < 0) {
            reason = 102;
            desc = "OVERLIMIT TRANSACTION";
        }
        // Legacy compares X(10) strings: blank expiration < any digit-leading
        // date, so a NULL expiration_date rejects too (unreachable in the
        // sample data — every account has one). NULL orig_ts compares as
        // spaces, which never exceed a real expiration date → no 103.
        LocalDate origDate = t.origTs() == null ? null : t.origTs().toLocalDate();
        boolean expired = origDate != null
                && (acct.expirationDate() == null || acct.expirationDate().isBefore(origDate));
        if (expired) {
            reason = 103;
            desc = "TRANSACTION RECEIVED AFTER ACCT EXPIRATION";
        }
        if (reason != 0) {
            reject(t, jobRunId, reason, desc);
            return Outcome.REJECTED;
        }

        // ---- 2000-POST-TRANSACTION ------------------------------------------
        // 2700-UPDATE-TCATBAL: +amt, create-if-missing (2700-A).
        jdbc.update("""
                        INSERT INTO transaction_category_balances (account_id, type_cd, category_cd, balance)
                        VALUES (?, ?, ?, ?)
                        ON CONFLICT (account_id, type_cd, category_cd)
                        DO UPDATE SET balance = transaction_category_balances.balance + EXCLUDED.balance
                        """,
                accountId, t.typeCd(), t.categoryCd(), t.amount());

        // 2800-UPDATE-ACCOUNT-REC.
        boolean credit = t.amount().signum() >= 0;
        int rows = jdbc.update("""
                        UPDATE accounts
                        SET curr_bal = curr_bal + ?,
                            curr_cyc_credit = curr_cyc_credit + ?,
                            curr_cyc_debit = curr_cyc_debit + ?,
                            version = version + 1
                        WHERE account_id = ?
                        """,
                t.amount(),
                credit ? t.amount() : BigDecimal.ZERO,
                credit ? BigDecimal.ZERO : t.amount(),
                accountId);
        if (rows == 0) {
            // See class javadoc: reason 109, transaction still written below.
            log.warn("account {} vanished mid-post (reason 109) for daily txn {}",
                    accountId, t.dailyTranId());
            reject(t, jobRunId, 109, "ACCOUNT RECORD NOT FOUND");
        }

        // 2900-WRITE-TRANSACTION-FILE: TRAN-ID = DALYTRAN-ID, proc_ts = now.
        jdbc.update("""
                        INSERT INTO transactions (transaction_id, type_cd, category_cd, source,
                          description, amount, merchant_id, merchant_name, merchant_city,
                          merchant_zip, card_number, orig_ts, proc_ts)
                        VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)
                        """,
                t.dailyTranId(), t.typeCd(), t.categoryCd(), t.source(), t.description(),
                t.amount(), t.merchantId(), t.merchantName(), t.merchantCity(),
                t.merchantZip(), t.cardNumber(), toTs(t.origTs()), Timestamp.valueOf(LocalDateTime.now()));
        return Outcome.POSTED;
    }

    /** 2500-WRITE-REJECT-REC: copy of the daily record + validation trailer. */
    private void reject(DailyTxn t, long jobRunId, int reason, String desc) {
        jdbc.update("""
                        INSERT INTO transaction_rejects (daily_tran_id, type_cd, category_cd, source,
                          description, amount, merchant_id, merchant_name, merchant_city,
                          merchant_zip, card_number, orig_ts, proc_ts, fail_reason,
                          fail_reason_desc, job_run_id)
                        VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
                        """,
                t.dailyTranId(), t.typeCd(), t.categoryCd(), t.source(), t.description(),
                t.amount(), t.merchantId(), t.merchantName(), t.merchantCity(),
                t.merchantZip(), t.cardNumber(), toTs(t.origTs()), toTs(t.procTs()),
                reason, desc, jobRunId);
    }

    private static Timestamp toTs(LocalDateTime v) {
        return v == null ? null : Timestamp.valueOf(v);
    }

    private record AcctRow(BigDecimal creditLimit, BigDecimal cycCredit,
                           BigDecimal cycDebit, LocalDate expirationDate) {
    }
}
