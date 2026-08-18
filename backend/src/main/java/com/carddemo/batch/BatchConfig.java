package com.carddemo.batch;

import com.carddemo.config.FilesConfig;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.item.database.JdbcCursorItemReader;
import org.springframework.batch.item.database.builder.JdbcCursorItemReaderBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.transaction.PlatformTransactionManager;

import javax.sql.DataSource;
import java.time.LocalDateTime;

/**
 * Spring Batch job wiring — Phase 4 (CBTRN02C / CBACT04C) and Phase 5
 * (CBTRN03C / CBSTM03A) jobs. Jobs never run on boot
 * ({@code spring.batch.job.enabled: false}); POSTTRAN / INTCALC / CREASTMT
 * are launched from {@code POST /api/v1/admin/jobs/{name}/run}, TRANREPT from
 * {@code POST /api/v1/reports/transaction} (the CORPT00C INTRDR replacement).
 */
@Configuration
public class BatchConfig {

    public static final String POST_TRANSACTIONS_JOB = "postTransactionsJob";
    public static final String INTEREST_CALC_JOB = "interestCalcJob";
    public static final String TRANSACTION_REPORT_JOB = "transactionReportJob";
    public static final String STATEMENT_JOB = "statementJob";

    // ---- POSTTRAN (CBTRN02C) ------------------------------------------------

    /**
     * Reads daily_transactions in {@code daily_tran_id} order — the fixture
     * file order (the legacy sequential DALYTRAN read; the derive script
     * asserts file order == id order).
     */
    @Bean
    public JdbcCursorItemReader<DailyTxn> dailyTxnReader(DataSource dataSource) {
        return new JdbcCursorItemReaderBuilder<DailyTxn>()
                .name("dailyTxnReader")
                .dataSource(dataSource)
                .sql("""
                        SELECT daily_tran_id, type_cd, category_cd, source, description,
                               amount, merchant_id, merchant_name, merchant_city,
                               merchant_zip, card_number, orig_ts, proc_ts
                        FROM daily_transactions
                        ORDER BY daily_tran_id
                        """)
                .rowMapper((rs, i) -> new DailyTxn(
                        rs.getString("daily_tran_id"),
                        rs.getString("type_cd"),
                        rs.getObject("category_cd", Integer.class),
                        rs.getString("source"),
                        rs.getString("description"),
                        rs.getBigDecimal("amount"),
                        rs.getObject("merchant_id", Long.class),
                        rs.getString("merchant_name"),
                        rs.getString("merchant_city"),
                        rs.getString("merchant_zip"),
                        rs.getString("card_number"),
                        rs.getObject("orig_ts", LocalDateTime.class),
                        rs.getObject("proc_ts", LocalDateTime.class)))
                .build();
    }

    /**
     * Validate-then-post per item INSIDE the writer — deliberately no
     * ItemProcessor: legacy 1500-B re-READs the account record that 2800 just
     * REWROTE, so record N's overlimit check must see record N-1's posted
     * amounts. A chunk-buffered processor would validate a whole chunk against
     * stale balances (see PostTranRecordService javadoc).
     */
    @Bean
    public Step postTransactionsStep(JobRepository jobRepository,
                                     PlatformTransactionManager transactionManager,
                                     JdbcCursorItemReader<DailyTxn> dailyTxnReader,
                                     PostTranRecordService postTranRecordService) {
        PostTranStepWriter writer = new PostTranStepWriter(postTranRecordService);
        return new StepBuilder("postTransactionsStep", jobRepository)
                .<DailyTxn, DailyTxn>chunk(50, transactionManager)
                .reader(dailyTxnReader)
                .writer(writer)
                .listener((org.springframework.batch.core.StepExecutionListener) writer)
                .build();
    }

    @Bean
    public Job postTransactionsJob(JobRepository jobRepository, Step postTransactionsStep) {
        return new JobBuilder(POST_TRANSACTIONS_JOB, jobRepository)
                .start(postTransactionsStep)
                .build();
    }

    // ---- INTCALC (CBACT04C) -------------------------------------------------

    @Bean
    public Step interestCalcStep(JobRepository jobRepository,
                                 PlatformTransactionManager transactionManager,
                                 JdbcTemplate jdbcTemplate) {
        return new StepBuilder("interestCalcStep", jobRepository)
                .tasklet(new InterestCalcTasklet(jdbcTemplate), transactionManager)
                .build();
    }

    @Bean
    public Job interestCalcJob(JobRepository jobRepository, Step interestCalcStep) {
        return new JobBuilder(INTEREST_CALC_JOB, jobRepository)
                .start(interestCalcStep)
                .build();
    }

    // ---- TRANREPT (CBTRN03C) — Phase 5 --------------------------------------

    @Bean
    public Step transactionReportStep(JobRepository jobRepository,
                                      PlatformTransactionManager transactionManager,
                                      JdbcTemplate jdbcTemplate,
                                      FilesConfig.FilesProperties filesProperties) {
        return new StepBuilder("transactionReportStep", jobRepository)
                .tasklet(new TransactionReportTasklet(jdbcTemplate, filesProperties.reportsDir()),
                        transactionManager)
                .build();
    }

    @Bean
    public Job transactionReportJob(JobRepository jobRepository, Step transactionReportStep) {
        return new JobBuilder(TRANSACTION_REPORT_JOB, jobRepository)
                .start(transactionReportStep)
                .build();
    }

    // ---- CREASTMT (CBSTM03A/B) — Phase 5 ------------------------------------

    @Bean
    public Step statementStep(JobRepository jobRepository,
                              PlatformTransactionManager transactionManager,
                              JdbcTemplate jdbcTemplate,
                              FilesConfig.FilesProperties filesProperties) {
        return new StepBuilder("statementStep", jobRepository)
                .tasklet(new StatementTasklet(jdbcTemplate, filesProperties.statementsDir()),
                        transactionManager)
                .build();
    }

    @Bean
    public Job statementJob(JobRepository jobRepository, Step statementStep) {
        return new JobBuilder(STATEMENT_JOB, jobRepository)
                .start(statementStep)
                .build();
    }
}
