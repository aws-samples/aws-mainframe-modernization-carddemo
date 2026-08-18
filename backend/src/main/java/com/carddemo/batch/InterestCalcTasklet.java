package com.carddemo.batch;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.jdbc.core.JdbcTemplate;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * CBACT04C (INTCALC) port — a single-threaded tasklet, because the legacy
 * program is one sequential sweep over TCATBALF in key order with
 * account-break state (WS-LAST-ACCT-NUM / WS-TOTAL-INT); chunk-parallel
 * processing would break the accumulation.
 *
 * <p>Per transaction_category_balances row (ordered by account_id, type_cd,
 * category_cd — the KSDS key order):</p>
 * <ul>
 *   <li><b>1200-GET-INTEREST-RATE</b>: rate from
 *       {@code disclosure_groups(acct.group_id, type_cd, category_cd)}; NULL
 *       group or lookup miss → retry with group {@code 'DEFAULT'}
 *       (1200-A-GET-DEFAULT-INT-RATE); a missing DEFAULT row FAILS THE JOB
 *       (legacy abends on any non-'00' status, CBACT04C.cbl:443-455 —
 *       abort-for-parity, plan §2.2 B1).</li>
 *   <li>rate = 0 → skip the row (no transaction, nothing added to the
 *       account total). A zero BALANCE with a nonzero rate still writes a
 *       0.00 transaction — 1300-B is unconditional inside 1300.</li>
 *   <li><b>1300-COMPUTE-INTEREST</b>:
 *       {@code monthly_int = balance * rate / 1200}, COBOL COMPUTE without
 *       ROUNDED → truncated toward zero at scale 2 ({@link RoundingMode#DOWN},
 *       plan R1; pinned by InterestRoundingTest).</li>
 *   <li><b>1300-B-WRITE-TX</b>: ONE transaction per tcatbal row —
 *       type '01', category 5 (MOVE '05' TO PIC 9(04) → 0005), source
 *       'System', desc 'Int. for a/c &lt;11-digit acct&gt;', merchant_id 0,
 *       merchant name/city/zip spaces (→ NULL per the schema's blank-is-NULL
 *       convention), card from xref by account id (lowest card number —
 *       matches the legacy alternate-index READ), orig_ts = proc_ts = now.
 *       TRAN-ID: legacy STRINGs PARM-DATE + a running suffix; replaced by
 *       the transaction_id sequence (plan R2 — the parm date's ONLY use is
 *       the id, so no job parameter is needed; parity compares rows by
 *       content, not id).</li>
 *   <li><b>1050-UPDATE-ACCOUNT</b> at account break:
 *       {@code curr_bal += total_int} and curr_cyc_credit/debit reset to 0
 *       for the PREVIOUS account (cycle reset stays inside this job — plan
 *       R7).</li>
 * </ul>
 *
 * <p><b>Legacy last-account quirk (replicated deliberately):</b> the CBACT04C
 * main loop is {@code PERFORM UNTIL END-OF-FILE = 'Y'} (test-before), so its
 * {@code ELSE PERFORM 1050-UPDATE-ACCOUNT} arm is unreachable dead code — the
 * LAST account's interest transactions are written but its curr_bal is never
 * incremented and its cycle fields are never reset. The golden files and this
 * port reproduce that behavior exactly (documented in
 * golden/derive_goldens.py); "fixing" it would be a parity break.</p>
 */
public class InterestCalcTasklet implements Tasklet {

    private static final Logger log = LoggerFactory.getLogger(InterestCalcTasklet.class);

    static final BigDecimal TWELVE_HUNDRED = new BigDecimal(1200);

    private final JdbcTemplate jdbc;

    public InterestCalcTasklet(JdbcTemplate jdbc) {
        this.jdbc = jdbc;
    }

    @Override
    public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) {
        List<TcatbalRow> rows = jdbc.query("""
                        SELECT t.account_id, t.type_cd, t.category_cd, t.balance, a.group_id
                        FROM transaction_category_balances t
                        JOIN accounts a ON a.account_id = t.account_id
                        ORDER BY t.account_id, t.type_cd, t.category_cd
                        """,
                (rs, i) -> new TcatbalRow(
                        rs.getLong("account_id"),
                        rs.getString("type_cd"),
                        rs.getInt("category_cd"),
                        rs.getBigDecimal("balance"),
                        rs.getString("group_id")));

        Map<Long, String> cardByAcct = new HashMap<>();
        long lastAcct = -1;
        BigDecimal totalInt = BigDecimal.ZERO;
        boolean first = true;
        int txnCount = 0;

        for (TcatbalRow row : rows) {
            if (row.accountId() != lastAcct) {
                if (!first) {
                    updateAccount(lastAcct, totalInt);   // 1050 for the PREVIOUS account
                }
                first = false;
                totalInt = BigDecimal.ZERO;
                lastAcct = row.accountId();
            }
            BigDecimal rate = interestRate(row);          // 1200 (+1200-A fallback)
            if (rate.signum() != 0) {
                BigDecimal monthlyInt = monthlyInterest(row.balance(), rate);
                totalInt = totalInt.add(monthlyInt);
                writeInterestTxn(row.accountId(), monthlyInt, cardByAcct);   // 1300-B
                txnCount++;
            }
        }
        // NO flush for the last account — legacy dead-code quirk (class javadoc).
        log.info("INTCALC: {} tcatbal rows read, {} interest transactions written; "
                + "last account {} intentionally not flushed (legacy parity)",
                rows.size(), txnCount, lastAcct);
        contribution.incrementWriteCount(txnCount);
        return RepeatStatus.FINISHED;
    }

    /**
     * 1300: {@code COMPUTE WS-MONTHLY-INT = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200}
     * — no ROUNDED clause, so COBOL truncates toward zero at the target's
     * scale 2 (plan R1; pinned by InterestRoundingTest).
     */
    static BigDecimal monthlyInterest(BigDecimal balance, BigDecimal rate) {
        return balance.multiply(rate).divide(TWELVE_HUNDRED, 2, RoundingMode.DOWN);
    }

    /** 1200-GET-INTEREST-RATE + 1200-A-GET-DEFAULT-INT-RATE (abends on miss). */
    private BigDecimal interestRate(TcatbalRow row) {
        if (row.groupId() != null) {
            List<BigDecimal> primary = jdbc.queryForList("""
                            SELECT interest_rate FROM disclosure_groups
                            WHERE group_id = ? AND type_cd = ? AND category_cd = ?
                            """,
                    BigDecimal.class, row.groupId(), row.typeCd(), row.categoryCd());
            if (!primary.isEmpty()) {
                return primary.get(0);
            }
            log.info("DISCLOSURE GROUP RECORD MISSING for ({}, {}, {}) — trying DEFAULT",
                    row.groupId(), row.typeCd(), row.categoryCd());
        }
        List<BigDecimal> fallback = jdbc.queryForList("""
                        SELECT interest_rate FROM disclosure_groups
                        WHERE group_id = 'DEFAULT' AND type_cd = ? AND category_cd = ?
                        """,
                BigDecimal.class, row.typeCd(), row.categoryCd());
        if (fallback.isEmpty()) {
            // 1200-A: legacy abends on any non-'00' status → job FAILS (plan B1).
            throw new IllegalStateException(
                    "ERROR READING DEFAULT DISCLOSURE GROUP: no DEFAULT row for type_cd="
                            + row.typeCd() + ", category_cd=" + row.categoryCd()
                            + " — aborting for parity (CBACT04C 1200-A abend)");
        }
        return fallback.get(0);
    }

    /** 1300-B-WRITE-TX — see class javadoc for the field-by-field mapping. */
    private void writeInterestTxn(long accountId, BigDecimal monthlyInt,
                                  Map<Long, String> cardByAcct) {
        String cardNumber = cardByAcct.computeIfAbsent(accountId, id -> {
            List<String> cards = jdbc.queryForList(
                    "SELECT card_number FROM cards WHERE account_id = ? ORDER BY card_number",
                    String.class, id);
            if (cards.isEmpty()) {
                // legacy 1110 only DISPLAYs on a miss but then abends on the bad
                // status; unreachable here (tcatbal FK → accounts, xref complete)
                throw new IllegalStateException("no card in xref for account " + id);
            }
            return cards.get(0);
        });
        long seq = jdbc.queryForObject("SELECT nextval('transaction_id_seq')", Long.class);
        Timestamp now = Timestamp.valueOf(LocalDateTime.now());
        jdbc.update("""
                        INSERT INTO transactions (transaction_id, type_cd, category_cd, source,
                          description, amount, merchant_id, merchant_name, merchant_city,
                          merchant_zip, card_number, orig_ts, proc_ts)
                        VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)
                        """,
                String.format("%016d", seq), "01", 5, "System",
                String.format("Int. for a/c %011d", accountId), monthlyInt,
                0L, null, null, null, cardNumber, now, now);
    }

    /** 1050-UPDATE-ACCOUNT: apply the account total, reset the cycle (plan R7). */
    private void updateAccount(long accountId, BigDecimal totalInt) {
        jdbc.update("""
                        UPDATE accounts
                        SET curr_bal = curr_bal + ?,
                            curr_cyc_credit = 0,
                            curr_cyc_debit = 0,
                            version = version + 1
                        WHERE account_id = ?
                        """,
                totalInt, accountId);
    }

    private record TcatbalRow(long accountId, String typeCd, int categoryCd,
                              BigDecimal balance, String groupId) {
    }
}
