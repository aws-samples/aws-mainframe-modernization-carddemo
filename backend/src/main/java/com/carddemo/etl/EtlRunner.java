package com.carddemo.etl;

import com.carddemo.crypto.SsnCrypto;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.transaction.support.TransactionTemplate;

/**
 * One-time ETL (plan §2.3), activated by the {@code etl} Spring profile:
 *
 * <pre>
 *   java -jar carddemo-backend.jar --spring.profiles.active=etl \
 *        [--etl.data-dir=...] [--etl.jcl-file=...] [--etl.seed-transactions=false]
 * </pre>
 *
 * The application exits after loading; a red validation report or any
 * integrity violation aborts with a non-zero exit code.
 */
@Configuration
@Profile("etl")
@EnableConfigurationProperties(EtlProperties.class)
public class EtlRunner {

    private static final Logger log = LoggerFactory.getLogger(EtlRunner.class);

    @Bean
    CommandLineRunner etlCommandLineRunner(JdbcTemplate jdbc,
                                           TransactionTemplate tx,
                                           EtlProperties props,
                                           SsnCrypto ssnCrypto) {
        return args -> {
            log.info("Starting CardDemo ETL (dataDir={}, jclFile={}, seedTransactions={})",
                    props.dataDir(), props.jclFile(), props.seedTransactions());
            EtlLoader loader = new EtlLoader(jdbc, props, new BCryptPasswordEncoder(), ssnCrypto);
            EtlValidationReport report = tx.execute(status -> {
                EtlValidationReport r = loader.load();
                if (!r.isGreen()) {
                    status.setRollbackOnly();
                }
                return r;
            });
            System.out.println(report.render());
            if (!report.isGreen()) {
                throw new EtlValidationException("ETL validation report is RED — load rolled back");
            }
        };
    }
}
