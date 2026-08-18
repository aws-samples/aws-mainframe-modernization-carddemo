package com.carddemo.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.nio.file.Path;

/**
 * Local output directory for batch-produced files (plan §1 "Object storage /
 * local dir"): transaction reports (CBTRN03C TRANREPT) under
 * {@code <dir>/reports} and statements (CBSTM03A STMTFILE/HTMLFILE) under
 * {@code <dir>/statements}. Replaces the legacy SYSOUT/GDG datasets; rows in
 * {@code report_runs} / {@code statements} index the files.
 */
@Configuration
@EnableConfigurationProperties(FilesConfig.FilesProperties.class)
public class FilesConfig {

    /** {@code carddemo.files.dir} — base directory, created on demand. */
    @ConfigurationProperties("carddemo.files")
    public record FilesProperties(String dir) {

        public Path reportsDir() {
            return Path.of(dir, "reports");
        }

        public Path statementsDir() {
            return Path.of(dir, "statements");
        }
    }
}
