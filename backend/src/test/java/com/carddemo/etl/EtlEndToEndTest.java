package com.carddemo.etl;

import org.flywaydb.core.Flyway;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * ETL end-to-end test (plan §8): Flyway V1 + full load against a real
 * PostgreSQL 16, asserting row counts (50 accounts / 50 customers / 50 cards /
 * 50 xref-backfilled FKs — the actual fixture line counts), checksums, the
 * blank-group→NULL rule, bcrypt users, the --seed-transactions flag, and the
 * xref 1:1 abort.
 */
@Testcontainers
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class EtlEndToEndTest {

    @Container
    static final PostgreSQLContainer<?> POSTGRES =
            new PostgreSQLContainer<>("postgres:16-alpine");

    static final Path ASCII = Path.of("../etl/fixtures/ascii");
    static final Path JCL = Path.of("../etl/fixtures/jcl/DUSRSECJ.jcl");

    JdbcTemplate jdbc;

    @BeforeEach
    void freshSchema() {
        var ds = new DriverManagerDataSource(
                POSTGRES.getJdbcUrl(), POSTGRES.getUsername(), POSTGRES.getPassword());
        jdbc = new JdbcTemplate(ds);
        Flyway.configure()
                .dataSource(ds)
                .cleanDisabled(false)
                .load()
                .clean();
        Flyway.configure().dataSource(ds).load().migrate();
    }

    private EtlValidationReport runEtl(boolean seedTransactions) {
        return runEtl(ASCII.toString(), seedTransactions);
    }

    private EtlValidationReport runEtl(String dataDir, boolean seedTransactions) {
        EtlProperties props = new EtlProperties(dataDir, JCL.toString(), seedTransactions);
        // low-strength bcrypt to keep the test fast; production default is used in EtlRunner
        return new EtlLoader(jdbc, props, new BCryptPasswordEncoder(4),
                new com.carddemo.crypto.SsnCrypto("test-only-ssn-key")).load();
    }

    private long count(String table) {
        Long n = jdbc.queryForObject("SELECT count(*) FROM " + table, Long.class);
        return n == null ? -1 : n;
    }

    @Test
    void fullLoadWithSeededTransactions() {
        EtlValidationReport report = runEtl(true);
        System.out.println(report.render());

        assertThat(report.violations()).isEmpty();
        assertThat(report.isGreen()).as("validation report green").isTrue();

        // fixture line counts: 50/50/50/50, 300 dailytran, 51 discgrp, 50 tcatbal, 18 trancatg, 7 trantype
        assertThat(count("accounts")).isEqualTo(50);
        assertThat(count("customers")).isEqualTo(50);
        assertThat(count("cards")).isEqualTo(50);
        assertThat(count("transaction_types")).isEqualTo(7);
        assertThat(count("transaction_categories")).isEqualTo(18);
        assertThat(count("disclosure_groups")).isEqualTo(51);
        assertThat(count("transaction_category_balances")).isEqualTo(50);
        assertThat(count("daily_transactions")).isEqualTo(300);
        assertThat(count("transactions")).isEqualTo(300);
        assertThat(count("users")).isEqualTo(10);

        // xref dissolution: every account got its customer_id backfilled (50 distinct accts in cardxref)
        assertThat(jdbc.queryForObject(
                "SELECT count(*) FROM accounts WHERE customer_id IS NOT NULL", Long.class))
                .isEqualTo(50);

        // B1: all 50 sample accounts have blank ACCT-GROUP-ID -> NULL
        assertThat(jdbc.queryForObject(
                "SELECT count(*) FROM accounts WHERE group_id IS NOT NULL", Long.class))
                .isEqualTo(0);

        // DEFAULT disclosure-group rows present for the INTCALC fallback
        assertThat(jdbc.queryForObject(
                "SELECT count(*) FROM disclosure_groups WHERE group_id = 'DEFAULT'", Long.class))
                .isEqualTo(17);

        // users: bcrypt hashes verifying against the legacy 8-char passwords
        var enc = new BCryptPasswordEncoder();
        var rows = jdbc.queryForList("SELECT user_id, password_hash, user_type FROM users");
        assertThat(rows).hasSize(10);
        for (var row : rows) {
            String hash = (String) row.get("password_hash");
            assertThat(hash).startsWith("$2");
            assertThat(enc.matches("PASSWORD", hash))
                    .as("bcrypt verifies for " + row.get("user_id")).isTrue();
        }
        assertThat(jdbc.queryForObject(
                "SELECT count(*) FROM users WHERE user_type = 'A'", Long.class)).isEqualTo(5);

        // spot-check a money checksum landed exactly (NUMERIC, no float drift)
        BigDecimal dbSum = jdbc.queryForObject(
                "SELECT SUM(amount) FROM daily_transactions", BigDecimal.class);
        BigDecimal txSum = jdbc.queryForObject(
                "SELECT SUM(amount) FROM transactions", BigDecimal.class);
        assertThat(dbSum).isEqualByComparingTo(txSum);
    }

    @Test
    void parityModeLeavesTransactionsEmpty() {
        EtlValidationReport report = runEtl(false);
        assertThat(report.isGreen()).isTrue();
        assertThat(count("transactions")).isZero();
        assertThat(count("daily_transactions")).isEqualTo(300);
    }

    @Nested
    class XrefAssertion {

        @Test
        void accountMappedToTwoCustomersAbortsLoudly(@org.junit.jupiter.api.io.TempDir Path tmp)
                throws Exception {
            // copy fixtures, then corrupt cardxref: point line 2's account at line 1's
            // account but keep its own (different) customer -> acct 50 maps to custs {50,27}
            for (String f : new String[]{"trantype.txt", "trancatg.txt", "discgrp.txt",
                    "custdata.txt", "acctdata.txt", "carddata.txt", "cardxref.txt",
                    "tcatbal.txt", "dailytran.txt"}) {
                Files.copy(ASCII.resolve(f), tmp.resolve(f));
            }
            var lines = Files.readAllLines(tmp.resolve("cardxref.txt"));
            String l2 = lines.get(1);
            // XREF-ACCT-ID occupies offsets 25..35 — overwrite with account 1's id
            String corrupted = l2.substring(0, 25) + "00000000050" + l2.substring(36);
            lines.set(1, corrupted);
            Files.write(tmp.resolve("cardxref.txt"), lines);

            assertThatThrownBy(() -> runEtl(tmp.toString(), true))
                    .isInstanceOf(EtlValidationException.class)
                    .hasMessageContaining("XREF 1:1 ASSERTION FAILED")
                    .hasMessageContaining("account 50");

            // nothing may have been committed silently — reference tables load first,
            // but the assertion runs before ANY insert, so all tables are empty
            assertThat(count("accounts")).isZero();
            assertThat(count("transaction_types")).isZero();
        }
    }

    @Test
    void fkViolationIsReportedNotDropped(@org.junit.jupiter.api.io.TempDir Path tmp) throws Exception {
        for (String f : new String[]{"trantype.txt", "trancatg.txt", "discgrp.txt",
                "custdata.txt", "acctdata.txt", "carddata.txt", "cardxref.txt",
                "tcatbal.txt", "dailytran.txt"}) {
            Files.copy(ASCII.resolve(f), tmp.resolve(f));
        }
        // corrupt carddata line 1: point CARD-ACCT-ID (offsets 16..26) at a missing account
        var lines = Files.readAllLines(tmp.resolve("carddata.txt"));
        String l1 = lines.get(0);
        lines.set(0, l1.substring(0, 16) + "99999999999" + l1.substring(27));
        Files.write(tmp.resolve("carddata.txt"), lines);

        EtlValidationReport report = runEtl(tmp.toString(), true);
        assertThat(report.isGreen()).as("FK violation must turn the report RED").isFalse();
        assertThat(report.violations())
                .anySatisfy(v -> assertThat(v).contains("account 99999999999 not in acctdata"));
    }

    @Test
    void flywaySchemaHasAll13Tables() {
        Long tables = jdbc.queryForObject("""
                SELECT count(*) FROM information_schema.tables
                WHERE table_schema = 'public'
                  AND table_name IN ('accounts','customers','cards','transactions',
                    'daily_transactions','transaction_types','transaction_categories',
                    'disclosure_groups','transaction_category_balances','users',
                    'transaction_rejects','statements','report_runs')
                """, Long.class);
        assertThat(tables).isEqualTo(13);

        // @Version columns per plan R3
        Long versionCols = jdbc.queryForObject("""
                SELECT count(*) FROM information_schema.columns
                WHERE table_schema = 'public' AND column_name = 'version'
                  AND table_name IN ('accounts','customers','cards','users')
                """, Long.class);
        assertThat(versionCols).isEqualTo(4);

        // AIX-replacement indexes
        Long idx = jdbc.queryForObject("""
                SELECT count(*) FROM pg_indexes WHERE schemaname = 'public'
                  AND indexname IN ('idx_cards_account_id','idx_transactions_card_number')
                """, Long.class);
        assertThat(idx).isEqualTo(2);
    }
}
