package com.carddemo.batch;

import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.StepExecutionListener;
import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ItemWriter;

/**
 * POSTTRAN step writer: runs {@link PostTranRecordService#process} per item,
 * in read order, inside the chunk transaction (see PostTranRecordService
 * javadoc for why this is a writer and not an ItemProcessor), and counts
 * posted/rejected records into the step's ExecutionContext
 * ({@code posted}/{@code rejected} — surfaced by GET /admin/jobs/runs).
 *
 * <p>Legacy CBTRN02C ends with RETURN-CODE 4 when any record was rejected but
 * still completes; mirrored here as exit status {@code COMPLETED_WITH_REJECTS}
 * (the job's BatchStatus stays COMPLETED).</p>
 */
public class PostTranStepWriter implements ItemWriter<DailyTxn>, StepExecutionListener {

    public static final String POSTED_KEY = "posted";
    public static final String REJECTED_KEY = "rejected";

    private final PostTranRecordService service;

    private StepExecution stepExecution;
    private int posted;
    private int rejected;

    public PostTranStepWriter(PostTranRecordService service) {
        this.service = service;
    }

    @Override
    public void beforeStep(StepExecution stepExecution) {
        this.stepExecution = stepExecution;
        this.posted = 0;
        this.rejected = 0;
    }

    @Override
    public void write(Chunk<? extends DailyTxn> chunk) {
        long jobRunId = stepExecution == null ? -1 : stepExecution.getJobExecutionId();
        for (DailyTxn t : chunk) {
            if (service.process(t, jobRunId) == PostTranRecordService.Outcome.POSTED) {
                posted++;
            } else {
                rejected++;
            }
        }
        if (stepExecution != null) {
            stepExecution.getExecutionContext().putInt(POSTED_KEY, posted);
            stepExecution.getExecutionContext().putInt(REJECTED_KEY, rejected);
        }
    }

    @Override
    public ExitStatus afterStep(StepExecution stepExecution) {
        // RETURN-CODE 4 analogue: completes, but flags that rejects were written.
        return rejected > 0
                ? new ExitStatus("COMPLETED_WITH_REJECTS",
                        rejected + " record(s) written to transaction_rejects")
                : ExitStatus.COMPLETED;
    }
}
