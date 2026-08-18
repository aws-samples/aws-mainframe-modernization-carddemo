package com.carddemo.api;

import com.carddemo.api.dto.BatchJobDtos.JobRunView;
import com.carddemo.batch.BatchConfig;
import com.carddemo.batch.PostTranStepWriter;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobExecutionException;
import org.springframework.batch.core.JobInstance;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.explore.JobExplorer;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Admin batch-job surface (plan §6): launch POSTTRAN / INTCALC on demand
 * (replaces the CICS-submitted JCL) and list job runs from Spring Batch's own
 * BATCH_* metadata (replaces GDG generation datasets, plan §1).
 */
@Service
public class BatchJobService {

    private final JobLauncher jobLauncher;
    private final JobExplorer jobExplorer;
    private final Map<String, Job> jobsByName;

    public BatchJobService(JobLauncher jobLauncher, JobExplorer jobExplorer, List<Job> jobs) {
        this.jobLauncher = jobLauncher;
        this.jobExplorer = jobExplorer;
        this.jobsByName = jobs.stream()
                .collect(Collectors.toMap(Job::getName, j -> j));
    }

    /**
     * Maps the URL segment (plan §6 path) to the Spring Batch job bean name.
     * transactionReportJob is deliberately NOT triggerable here — it is
     * launched through POST /reports/transaction (the CORPT00C flow), which
     * owns its report_runs bookkeeping and job parameters.
     */
    private static String jobBeanName(String pathName) {
        return switch (pathName) {
            case "postTransactions" -> BatchConfig.POST_TRANSACTIONS_JOB;
            case "interestCalc" -> BatchConfig.INTEREST_CALC_JOB;
            case "statements" -> BatchConfig.STATEMENT_JOB;
            default -> throw new NotFoundException("Unknown batch job: " + pathName);
        };
    }

    /**
     * Launches the job synchronously (demo scale: 300 daily transactions) with
     * a unique timestamp parameter so re-runs create new JobInstances.
     */
    public JobRunView run(String pathName) {
        Job job = jobsByName.get(jobBeanName(pathName));
        if (job == null) {
            throw new NotFoundException("Batch job not wired: " + pathName);
        }
        JobParameters params = new JobParametersBuilder()
                .addLong("requestedAt", System.currentTimeMillis())
                .toJobParameters();
        try {
            JobExecution execution = jobLauncher.run(job, params);
            return toView(job.getName(), execution);
        } catch (JobExecutionException e) {
            // unreachable with a unique timestamp param; surfaces as 409 if it ever fires
            throw new ConflictException("Could not launch job: " + e.getMessage());
        }
    }

    /** All runs of every job, newest first, straight from the BATCH_* tables. */
    public List<JobRunView> listRuns() {
        List<JobRunView> runs = new ArrayList<>();
        for (String jobName : List.of(BatchConfig.POST_TRANSACTIONS_JOB, BatchConfig.INTEREST_CALC_JOB,
                BatchConfig.STATEMENT_JOB, BatchConfig.TRANSACTION_REPORT_JOB)) {
            for (JobInstance instance : jobExplorer.getJobInstances(jobName, 0, 100)) {
                for (JobExecution execution : jobExplorer.getJobExecutions(instance)) {
                    runs.add(toView(jobName, execution));
                }
            }
        }
        runs.sort(Comparator.comparing(JobRunView::runId).reversed());
        return runs;
    }

    private static JobRunView toView(String jobName, JobExecution execution) {
        long readCount = 0;
        long writeCount = 0;
        Integer posted = null;
        Integer rejected = null;
        for (StepExecution step : execution.getStepExecutions()) {
            readCount += step.getReadCount();
            writeCount += step.getWriteCount();
            if (step.getExecutionContext().containsKey(PostTranStepWriter.POSTED_KEY)) {
                posted = step.getExecutionContext().getInt(PostTranStepWriter.POSTED_KEY);
                rejected = step.getExecutionContext().getInt(PostTranStepWriter.REJECTED_KEY);
            }
        }
        LocalDateTime start = execution.getStartTime();
        LocalDateTime end = execution.getEndTime();
        return new JobRunView(
                execution.getId(),
                jobName,
                execution.getStatus().name(),
                execution.getExitStatus().getExitCode(),
                start,
                end,
                readCount,
                writeCount,
                posted,
                rejected);
    }
}
