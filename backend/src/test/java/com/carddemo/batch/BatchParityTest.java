package com.carddemo.batch;

import com.carddemo.etl.EtlLoader;
import com.carddemo.etl.EtlProperties;
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
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.test.context.TestPropertySource;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.util.List;

import static com.carddemo.batch.GoldenFiles.dump;
import static com.carddemo.batch.GoldenFiles.golden;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * Golden-file parity for the two Phase 4 batch jobs (plan §4 + §7 exit
 * criteria): seed the real ASCII fixtures WITHOUT the dailytran→transactions
 * pre-load (§2.3 Phase 4 caveat — CBTRN02C posts with TRAN-ID = DALYTRAN-ID,
 * so the browsing pre-seed would collide on all 300 ids), run
 * postTransactionsJob then interestCalcJob, dump the four output tables, and
 * diff them line-by-line against goldens derived independently of this code
 * by golden/derive_goldens.py (see GoldenFiles javadoc).
 *
 * <p>Expected from the shipped sample data: 262 posted / 38 rejected — all
 * reason 102 (the accounts expire 2023–2025, after the 2022-06-10 orig
 * timestamps, so no 103s arise from the sample; the synthetic-case tests
 * cover 100/101/103). INTCALC: 50 interest transactions (one per tcatbal
 * row, all falling back to the DEFAULT disclosure group because every sample
 * account has a NULL group), account 50 deliberately left unflushed (legacy
 * CBACT04C dead-code quirk — see InterestCalcTasklet javadoc).</p>
 */
@Testcontainers
@SpringBootTest
@TestPropertySource(properties = {
        "carddemo.jwt.secret=test-only-jwt-signing-secret-32-bytes-min!",
        "carddemo.jwt.ttl=15m"
})
class BatchParityTest {

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

    @Test
    void postTranThenInterestCalcMatchIndependentlyDerivedGoldens() throws Exception {
        seedParityFixtures();

        // ---- POSTTRAN (CBTRN02C) -------------------------------------------
        JobExecution postTran = launch(postTransactionsJob);
        assertThat(postTran.getStatus()).isEqualTo(BatchStatus.COMPLETED);
        // RETURN-CODE 4 analogue: rejects were written but the job completed
        assertThat(postTran.getExitStatus().getExitCode()).isEqualTo("COMPLETED_WITH_REJECTS");
        var stepCtx = postTran.getStepExecutions().iterator().next().getExecutionContext();
        assertThat(stepCtx.getInt(PostTranStepWriter.POSTED_KEY)).isEqualTo(262);
        assertThat(stepCtx.getInt(PostTranStepWriter.REJECTED_KEY)).isEqualTo(38);

        // proc_ts is run-time-dependent and excluded from the goldens
        diff("posttran/transactions.tsv", dump(jdbc,
                "transaction_id\ttype_cd\tcategory_cd\tsource\tdescription\tamount\tmerchant_id"
                        + "\tmerchant_name\tmerchant_city\tmerchant_zip\tcard_number\torig_ts",
                """
                SELECT transaction_id, type_cd, category_cd, source, description, amount,
                       merchant_id, merchant_name, merchant_city, merchant_zip, card_number, orig_ts
                FROM transactions ORDER BY transaction_id
                """));
        diff("posttran/transaction_rejects.tsv", dump(jdbc,
                "daily_tran_id\ttype_cd\tcategory_cd\tsource\tdescription\tamount\tmerchant_id"
                        + "\tmerchant_name\tmerchant_city\tmerchant_zip\tcard_number\torig_ts"
                        + "\tfail_reason\tfail_reason_desc",
                """
                SELECT daily_tran_id, type_cd, category_cd, source, description, amount,
                       merchant_id, merchant_name, merchant_city, merchant_zip, card_number,
                       orig_ts, fail_reason, fail_reason_desc
                FROM transaction_rejects ORDER BY daily_tran_id
                """));
        diff("posttran/accounts.tsv", dumpAccounts());
        diff("posttran/transaction_category_balances.tsv", dump(jdbc,
                "account_id\ttype_cd\tcategory_cd\tbalance",
                """
                SELECT account_id, type_cd, category_cd, balance
                FROM transaction_category_balances ORDER BY account_id, type_cd, category_cd
                """));

        // every reject row is tagged with the launching job execution
        assertThat(jdbc.queryForObject(
                "SELECT count(*) FROM transaction_rejects WHERE job_run_id = ?",
                Long.class, postTran.getId())).isEqualTo(38L);

        // ---- INTCALC (CBACT04C), running on the post-POSTTRAN state ---------
        // DEFAULT-group fallback is exercised by EVERY row: all 50 sample
        // accounts have a blank→NULL group_id (asserted here, plan §7).
        assertThat(jdbc.queryForObject(
                "SELECT count(*) FROM accounts WHERE group_id IS NULL", Long.class))
                .isEqualTo(50L);

        JobExecution intCalc = launch(interestCalcJob);
        assertThat(intCalc.getStatus()).isEqualTo(BatchStatus.COMPLETED);
        assertThat(intCalc.getStepExecutions().iterator().next().getWriteCount()).isEqualTo(50L);

        // interest rows compared by content, not id/timestamps (plan R2:
        // sequence ids replace the legacy PARM-DATE-derived TRAN-IDs)
        diff("intcalc/interest_transactions.tsv", dump(jdbc,
                "type_cd\tcategory_cd\tsource\tdescription\tamount\tmerchant_id"
                        + "\tmerchant_name\tmerchant_city\tmerchant_zip\tcard_number",
                """
                SELECT type_cd, category_cd, source, description, amount, merchant_id,
                       merchant_name, merchant_city, merchant_zip, card_number
                FROM transactions WHERE source = 'System'
                ORDER BY description, type_cd, category_cd
                """));
        diff("intcalc/accounts.tsv", dumpAccounts());
    }

    private void seedParityFixtures() {
        // §2.3 parity mode: seedTransactions = false (empty transactions table)
        EtlProperties props = new EtlProperties(
                "../etl/fixtures/ascii", "../etl/fixtures/jcl/DUSRSECJ.jcl", false);
        new EtlLoader(jdbc, props, new BCryptPasswordEncoder(4),
                new com.carddemo.crypto.SsnCrypto("test-only-ssn-key")).load();
        assertThat(jdbc.queryForObject("SELECT count(*) FROM transactions", Long.class))
                .as("parity seed must NOT pre-load transactions (plan §2.3)")
                .isEqualTo(0L);
        assertThat(jdbc.queryForObject("SELECT count(*) FROM daily_transactions", Long.class))
                .isEqualTo(300L);
    }

    private JobExecution launch(Job job) throws Exception {
        return jobLauncher.run(job, new JobParametersBuilder()
                .addLong("requestedAt", System.nanoTime())
                .toJobParameters());
    }

    private List<String> dumpAccounts() {
        return dump(jdbc, "account_id\tcurr_bal\tcurr_cyc_credit\tcurr_cyc_debit",
                "SELECT account_id, curr_bal, curr_cyc_credit, curr_cyc_debit"
                        + " FROM accounts ORDER BY account_id");
    }

    private void diff(String goldenPath, List<String> actual) {
        assertThat(actual)
                .as("table dump vs golden %s", goldenPath)
                .containsExactlyElementsOf(golden(goldenPath));
    }
}
