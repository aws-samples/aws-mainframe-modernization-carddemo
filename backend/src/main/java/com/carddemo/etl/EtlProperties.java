package com.carddemo.etl;

import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * ETL configuration. {@code seedTransactions} implements the plan §2.3
 * {@code --seed-transactions} flag: ON (default) pre-loads dailytran.txt into
 * {@code transactions} for demo browsing; OFF leaves {@code transactions}
 * empty for POSTTRAN parity fixtures (Phase 4 seeding caveat).
 */
@ConfigurationProperties(prefix = "etl")
public record EtlProperties(
        String dataDir,
        String jclFile,
        boolean seedTransactions) {

    public EtlProperties {
        if (dataDir == null) {
            dataDir = "../etl/fixtures/ascii";
        }
        if (jclFile == null) {
            jclFile = "../etl/fixtures/jcl/DUSRSECJ.jcl";
        }
    }
}
