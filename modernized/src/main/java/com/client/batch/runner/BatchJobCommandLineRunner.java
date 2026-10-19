package com.client.batch.runner;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.*;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

/**
 * Translates Spring Batch ExitStatus into IBM Mainframe JCL Condition Codes (RC):
 * - RC 0:  COMPLETED (Success)
 * - RC 4:  WARNING / STOPPED (Step warning)
 * - RC 8:  FAILED (Recoverable job error)
 * - RC 12: SEVERE_ERROR (Step abnormal termination)
 * - RC 16: FATAL_ERROR (Infrastructure or database failure)
 */
@Component
public class BatchJobCommandLineRunner implements CommandLineRunner {
    private static final Logger log = LoggerFactory.getLogger(BatchJobCommandLineRunner.class);

    private final ObjectProvider<JobLauncher> jobLauncherProvider;
    private final ApplicationContext context;

    public BatchJobCommandLineRunner(ObjectProvider<JobLauncher> jobLauncherProvider, ApplicationContext context) {
        this.jobLauncherProvider = jobLauncherProvider;
        this.context = context;
    }

    @Override
    public void run(String... args) throws Exception {
        String jobName = null;
        for (String arg : args) {
            if (arg.startsWith("--job.name=")) {
                jobName = arg.substring("--job.name=".length());
            }
        }
        if (jobName == null) {
            log.info("No --job.name specified. Running in online Web server mode.");
            return;
        }

        log.info("Initiating batch execution for JCL Job: {}", jobName);
        int exitCode = executeJob(jobName);
        log.info("Batch Job {} finished with JCL Condition Code: RC {}", jobName, exitCode);
        System.exit(exitCode);
    }

    public int executeJob(String jobName) {
        try {
            JobLauncher jobLauncher = jobLauncherProvider.getIfAvailable();
            if (jobLauncher == null) {
                log.warn("JobLauncher bean not available; skipping batch execution for {}", jobName);
                return 0;
            }
            Job job = context.getBean(jobName, Job.class);
            JobParameters params = new JobParametersBuilder()
                .addLong("timestamp", System.currentTimeMillis())
                .toJobParameters();
            JobExecution execution = jobLauncher.run(job, params);
            BatchStatus status = execution.getStatus();

            if (status == BatchStatus.COMPLETED) {
                return 0; // JCL RC 0 (Success)
            } else if (status == BatchStatus.STOPPED) {
                return 4; // JCL RC 4 (Warning)
            } else if (status == BatchStatus.FAILED) {
                return 8; // JCL RC 8 (Error)
            } else {
                return 12; // JCL RC 12 (Severe Error)
            }
        } catch (Exception e) {
            log.error("Fatal error executing batch job {}", jobName, e);
            return 16; // JCL RC 16 (Fatal Error)
        }
    }
}
