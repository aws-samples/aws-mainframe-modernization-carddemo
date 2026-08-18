package com.carddemo.batch;

import com.carddemo.etl.EtlLoader;
import com.carddemo.etl.EtlProperties;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
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
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.springframework.test.context.TestPropertySource;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static com.carddemo.batch.GoldenFiles.golden;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * Golden-file parity for the Phase 5 batch outputs (plan §4/§7):
 * <ol>
 *   <li>seed the ASCII fixtures WITHOUT the transactions pre-load, run
 *       POSTTRAN (posted = 262, as in BatchParityTest);</li>
 *   <li>run the CBTRN03C report over 2020-01-01..2099-12-31 (proc_ts is the
 *       run timestamp, so every posted row qualifies) and diff the full
 *       133-char report against golden/report/tranrept.txt;</li>
 *   <li>run the CBSTM03A statement job and diff the plain-text statements of
 *       the two golden accounts (50 and 27 — the first two cards in
 *       card-number order) against golden/statements/stmt-*.txt, plus a
 *       structural check of the HTML rendition.</li>
 * </ol>
 *
 * <p>The goldens were derived INDEPENDENTLY of the Java under test by
 * golden/derive_report_statement_goldens.py — a hand-executed port of the
 * CBTRN03C line accounting (incl. the EOF re-add quirk and the dropped last
 * account total) and the CBSTM03A ST-LINE layouts (incl. the
 * DELIMITED-BY-space name/address quirk and the PIC Z(9).99- / 9(9).99-
 * editing) straight from the COBOL and the raw fixture files. Timestamps
 * never appear in either layout, so the diff is exact.</p>
 */
@Testcontainers
@SpringBootTest
@TestPropertySource(properties = {
        "carddemo.jwt.secret=test-only-jwt-signing-secret-32-bytes-min!",
        "carddemo.jwt.ttl=15m"
})
class ReportStatementParityTest {

    @Container
    @ServiceConnection
    static final PostgreSQLContainer<?> POSTGRES = new PostgreSQLContainer<>("postgres:16-alpine");

    @TempDir
    static Path filesDir;

    @DynamicPropertySource
    static void filesDir(DynamicPropertyRegistry registry) {
        registry.add("carddemo.files.dir", () -> filesDir.toString());
    }

    @Autowired
    JdbcTemplate jdbc;
    @Autowired
    JobLauncher jobLauncher;
    @Autowired
    @Qualifier(BatchConfig.POST_TRANSACTIONS_JOB)
    Job postTransactionsJob;
    @Autowired
    @Qualifier(BatchConfig.TRANSACTION_REPORT_JOB)
    Job transactionReportJob;
    @Autowired
    @Qualifier(BatchConfig.STATEMENT_JOB)
    Job statementJob;

    @Test
    void reportAndStatementsMatchIndependentlyDerivedGoldens() throws Exception {
        seedParityFixtures();

        // ---- POSTTRAN first: the report filters on proc_ts (run time) ------
        JobExecution postTran = jobLauncher.run(postTransactionsJob, new JobParametersBuilder()
                .addLong("requestedAt", System.nanoTime())
                .toJobParameters());
        assertThat(postTran.getExitStatus().getExitCode()).isEqualTo("COMPLETED_WITH_REJECTS");
        assertThat(jdbc.queryForObject("SELECT count(*) FROM transactions", Long.class))
                .isEqualTo(262L);

        // ---- TRANREPT (CBTRN03C) -------------------------------------------
        Long runId = jdbc.queryForObject("""
                        INSERT INTO report_runs (report_type, from_date, to_date, status, requested_by)
                        VALUES ('custom', DATE '2020-01-01', DATE '2099-12-31', 'PENDING', 'ADMIN001')
                        RETURNING id
                        """, Long.class);
        JobExecution report = jobLauncher.run(transactionReportJob, new JobParametersBuilder()
                .addLong("reportRunId", runId)
                .addString("fromDate", "2020-01-01")
                .addString("toDate", "2099-12-31")
                .addLong("requestedAt", System.nanoTime())
                .toJobParameters());
        assertThat(report.getStatus()).isEqualTo(BatchStatus.COMPLETED);
        assertThat(jdbc.queryForObject(
                "SELECT status FROM report_runs WHERE id = ?", String.class, runId))
                .isEqualTo("COMPLETED");

        Path reportFile = Path.of(jdbc.queryForObject(
                "SELECT file_path FROM report_runs WHERE id = ?", String.class, runId));
        assertThat(readLines(reportFile))
                .as("133-char CBTRN03C report vs independently derived golden")
                .containsExactlyElementsOf(golden("report/tranrept.txt"));

        // ---- CREASTMT (CBSTM03A) --------------------------------------------
        JobExecution stmts = jobLauncher.run(statementJob, new JobParametersBuilder()
                .addString("cycleDate", "2022-07-01")
                .addLong("requestedAt", System.nanoTime())
                .toJobParameters());
        assertThat(stmts.getStatus()).isEqualTo(BatchStatus.COMPLETED);
        // one statement per card-xref record (50 cards / 50 accounts)
        assertThat(stmts.getStepExecutions().iterator().next().getWriteCount()).isEqualTo(50L);
        assertThat(jdbc.queryForObject("SELECT count(*) FROM statements", Long.class))
                .isEqualTo(50L);

        // txt parity for the two golden accounts (first two cards by number)
        for (long accountId : new long[]{50, 27}) {
            assertThat(readLines(statementPath(accountId, "txt_path")))
                    .as("80-char CBSTM03A statement for account %d vs golden", accountId)
                    .containsExactlyElementsOf(golden("statements/stmt-" + accountId + ".txt"));
        }

        // HTML structural check (plan R4: content parity, not byte parity)
        List<String> html = readLines(statementPath(50, "html_path"));
        assertThat(html).allSatisfy(line -> assertThat(line)
                .as("FD-HTMLFILE-REC is PIC X(100)").hasSize(100));
        assertThat(html.get(0)).startsWith("<!DOCTYPE html>");
        assertThat(html).anySatisfy(l -> assertThat(l)
                .startsWith("<h3>Statement for Account Number: 00000000050"));
        assertThat(html).anySatisfy(l -> assertThat(l).startsWith("<p>Account ID         : 00000000050"));
        assertThat(html).anySatisfy(l -> assertThat(l).startsWith("<p>Current Balance    : 000001945.87"));
        // one L58 cell (Tran ID column) per transaction: account 50 has 6
        assertThat(html.stream().filter(l -> l.startsWith(
                "<td style=\"width:25%; padding:0px 5px; background-color:#f2f2f2; text-align:left;\">"))
                .count()).isEqualTo(6L);
        assertThat(html).anySatisfy(l -> assertThat(l).startsWith("<h3>End of Statement</h3>"));
        assertThat(html.get(html.size() - 1)).startsWith("</html>");
    }

    private Path statementPath(long accountId, String column) {
        return Path.of(jdbc.queryForObject(
                "SELECT " + column + " FROM statements WHERE account_id = ?",
                String.class, accountId));
    }

    private static List<String> readLines(Path file) {
        try {
            return Files.readAllLines(file, StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    private void seedParityFixtures() {
        EtlProperties props = new EtlProperties(
                "../etl/fixtures/ascii", "../etl/fixtures/jcl/DUSRSECJ.jcl", false);
        new EtlLoader(jdbc, props, new BCryptPasswordEncoder(4),
                new com.carddemo.crypto.SsnCrypto("test-only-ssn-key")).load();
        assertThat(jdbc.queryForObject("SELECT count(*) FROM transactions", Long.class))
                .as("parity seed must NOT pre-load transactions (plan §2.3)")
                .isEqualTo(0L);
    }
}
