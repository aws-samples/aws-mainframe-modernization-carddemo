package com.carddemo.batch;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.testcontainers.service.connection.ServiceConnection;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.TestPropertySource;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.math.BigDecimal;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Synthetic batch fixtures (plan §4 golden-file strategy + §7 exit criteria):
 * the shipped sample data cannot produce reject reasons 100 or 101 (every
 * dailytran card exists in the xref and every xref account exists), and its
 * accounts expire 2023–2025 — after the 2022-06-10 orig timestamps — so no
 * 103 arises either. These tests hand-seed minimal rows to cover:
 * <ul>
 *   <li>100 — card not in xref (CBTRN02C 1500-A);</li>
 *   <li>101 — xref card pointing at a missing account (1500-B INVALID KEY;
 *       the cards→accounts FK is dropped for this seed, since the legacy
 *       VSAM files had no such integrity);</li>
 *   <li>103 overwrites 102 — a transaction that is BOTH over limit AND past
 *       expiration must reject with 103, proving last-wins (CBTRN02C.cbl
 *       403-421), and processing continues to the next record;</li>
 *   <li>missing DEFAULT disclosure group — interestCalcJob FAILS
 *       (abort-for-parity, CBACT04C 1200-A abend);</li>
 *   <li>rate = 0 — tcatbal row skipped, no interest transaction.</li>
 * </ul>
 */
@Testcontainers
@SpringBootTest
@TestPropertySource(properties = {
        "carddemo.jwt.secret=test-only-jwt-signing-secret-32-bytes-min!",
        "carddemo.jwt.ttl=15m"
})
class BatchSyntheticCasesTest {

    @Container
    @ServiceConnection
    static final PostgreSQLContainer<?> POSTGRES = new PostgreSQLContainer<>("postgres:16-alpine");

    @Autowired
    JdbcTemplate jdbc;
    @Autowired
    JobLauncher jobLauncher;
    @Autowired
    @Qualifier("postTransactionsJob")
    Job postTransactionsJob;
    @Autowired
    @Qualifier("interestCalcJob")
    Job interestCalcJob;

    @BeforeEach
    void cleanBusinessTables() {
        // fresh business state per test; BATCH_* metadata may accumulate (each
        // launch uses a unique parameter, so re-runs create new instances)
        jdbc.update("DELETE FROM transaction_rejects");
        jdbc.update("DELETE FROM transactions");
        jdbc.update("DELETE FROM daily_transactions");
        jdbc.update("DELETE FROM transaction_category_balances");
        jdbc.update("DELETE FROM cards");
        jdbc.update("DELETE FROM accounts");
        jdbc.update("DELETE FROM disclosure_groups");
    }

    // ---- POSTTRAN reject reasons -------------------------------------------

    @Test
    void unknownCardRejectsWithReason100() throws Exception {
        seedAccountAndCard(1L, "1111222233334444", "5000.00", "2030-12-31");
        seedDailyTxn("0000000000000001", "9999999999999999", "10.00"); // no such card
        seedDailyTxn("0000000000000002", "1111222233334444", "10.00"); // valid — must still post

        JobExecution run = launch(postTransactionsJob);
        assertThat(run.getStatus()).isEqualTo(BatchStatus.COMPLETED);

        Map<String, Object> reject = jdbc.queryForMap(
                "SELECT fail_reason, fail_reason_desc FROM transaction_rejects");
        assertThat(reject.get("fail_reason")).isEqualTo(100);
        assertThat(reject.get("fail_reason_desc")).isEqualTo("INVALID CARD NUMBER FOUND");
        // processing continued past the reject (legacy main loop reads next)
        assertThat(count("transactions")).isEqualTo(1L);
    }

    @Test
    void cardWithMissingAccountRejectsWithReason101() throws Exception {
        // legacy XREF could point at a nonexistent ACCTDAT record; the target
        // schema forbids that via FK, so the synthetic seed drops it
        jdbc.execute("ALTER TABLE cards DROP CONSTRAINT fk_cards_account");
        try {
            jdbc.update("""
                    INSERT INTO cards (card_number, account_id, active_status)
                    VALUES ('1111222233334444', 424242, 'Y')
                    """);
            seedDailyTxn("0000000000000001", "1111222233334444", "10.00");

            JobExecution run = launch(postTransactionsJob);
            assertThat(run.getStatus()).isEqualTo(BatchStatus.COMPLETED);

            Map<String, Object> reject = jdbc.queryForMap(
                    "SELECT fail_reason, fail_reason_desc FROM transaction_rejects");
            assertThat(reject.get("fail_reason")).isEqualTo(101);
            assertThat(reject.get("fail_reason_desc")).isEqualTo("ACCOUNT RECORD NOT FOUND");
            assertThat(count("transactions")).isEqualTo(0L);
        } finally {
            // remove the orphan row before restoring schema integrity
            jdbc.update("DELETE FROM cards WHERE card_number = '1111222233334444'");
            jdbc.execute("ALTER TABLE cards ADD CONSTRAINT fk_cards_account"
                    + " FOREIGN KEY (account_id) REFERENCES accounts (account_id)");
        }
    }

    @Test
    void overlimitAndExpiredRejectsWith103LastWins() throws Exception {
        // credit_limit 100 < 0 - 0 + 500 → 102 fires; expiration 2020-01-01 <
        // orig 2022-06-10 → 103 fires AFTER and overwrites (CBTRN02C.cbl:403-421)
        seedAccountAndCard(1L, "1111222233334444", "100.00", "2020-01-01");
        seedDailyTxn("0000000000000001", "1111222233334444", "500.00");

        JobExecution run = launch(postTransactionsJob);
        assertThat(run.getStatus()).isEqualTo(BatchStatus.COMPLETED);

        Map<String, Object> reject = jdbc.queryForMap(
                "SELECT fail_reason, fail_reason_desc FROM transaction_rejects");
        assertThat(reject.get("fail_reason")).isEqualTo(103);
        assertThat(reject.get("fail_reason_desc"))
                .isEqualTo("TRANSACTION RECEIVED AFTER ACCT EXPIRATION");
        assertThat(count("transactions")).isEqualTo(0L);
        // rejected record must not touch the account
        assertThat(jdbc.queryForObject(
                "SELECT curr_bal FROM accounts WHERE account_id = 1", BigDecimal.class))
                .isEqualByComparingTo("0.00");
    }

    @Test
    void overlimitAloneRejectsWith102() throws Exception {
        seedAccountAndCard(1L, "1111222233334444", "100.00", "2030-12-31");
        seedDailyTxn("0000000000000001", "1111222233334444", "500.00");

        launch(postTransactionsJob);

        assertThat(jdbc.queryForObject(
                "SELECT fail_reason FROM transaction_rejects", Integer.class)).isEqualTo(102);
    }

    // ---- INTCALC edge cases --------------------------------------------------

    @Test
    void missingDefaultDisclosureGroupFailsTheJob() throws Exception {
        seedAccountAndCard(1L, "1111222233334444", "5000.00", "2030-12-31");
        jdbc.update("""
                INSERT INTO transaction_category_balances (account_id, type_cd, category_cd, balance)
                VALUES (1, '01', 1, 100.00)
                """);
        // no disclosure_groups rows at all → primary miss AND no DEFAULT

        JobExecution run = launch(interestCalcJob);

        // legacy CBACT04C 1200-A abends on any non-'00' status → job FAILS
        assertThat(run.getStatus()).isEqualTo(BatchStatus.FAILED);
        assertThat(run.getAllFailureExceptions())
                .anySatisfy(e -> assertThat(e.getMessage())
                        .contains("ERROR READING DEFAULT DISCLOSURE GROUP"));
        assertThat(count("transactions")).isEqualTo(0L);
    }

    @Test
    void zeroRateSkipsTheRowWithoutWritingATransaction() throws Exception {
        seedAccountAndCard(1L, "1111222233334444", "5000.00", "2030-12-31");
        jdbc.update("""
                INSERT INTO transaction_category_balances (account_id, type_cd, category_cd, balance)
                VALUES (1, '01', 1, 100.00)
                """);
        jdbc.update("""
                INSERT INTO disclosure_groups (group_id, type_cd, category_cd, interest_rate)
                VALUES ('DEFAULT', '01', 1, 0.00)
                """);

        JobExecution run = launch(interestCalcJob);

        assertThat(run.getStatus()).isEqualTo(BatchStatus.COMPLETED);
        assertThat(count("transactions")).isEqualTo(0L);
    }

    // ---- helpers --------------------------------------------------------------

    private void seedAccountAndCard(long accountId, String cardNumber,
                                    String creditLimit, String expiration) {
        jdbc.update("""
                INSERT INTO accounts (account_id, active_status, curr_bal, credit_limit,
                                      expiration_date, curr_cyc_credit, curr_cyc_debit)
                VALUES (?, 'Y', 0, ?::numeric, ?::date, 0, 0)
                """, accountId, creditLimit, expiration);
        jdbc.update("""
                INSERT INTO cards (card_number, account_id, active_status)
                VALUES (?, ?, 'Y')
                """, cardNumber, accountId);
    }

    private void seedDailyTxn(String id, String cardNumber, String amount) {
        jdbc.update("""
                INSERT INTO daily_transactions (daily_tran_id, type_cd, category_cd, source,
                                                description, amount, card_number, orig_ts)
                VALUES (?, '01', 1, 'POS TERM', 'synthetic', ?::numeric, ?,
                        '2022-06-10 19:27:53'::timestamp)
                """, id, amount, cardNumber);
    }

    private JobExecution launch(Job job) throws Exception {
        return jobLauncher.run(job, new JobParametersBuilder()
                .addLong("requestedAt", System.nanoTime())
                .toJobParameters());
    }

    private long count(String table) {
        Long n = jdbc.queryForObject("SELECT count(*) FROM " + table, Long.class);
        return n == null ? -1 : n;
    }
}
