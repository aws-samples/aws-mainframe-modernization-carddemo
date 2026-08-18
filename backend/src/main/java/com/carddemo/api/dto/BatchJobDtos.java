package com.carddemo.api.dto;

import java.time.LocalDateTime;

/** Admin batch-job DTOs (plan §6 — /admin/jobs endpoints). */
public final class BatchJobDtos {

    private BatchJobDtos() {
    }

    /**
     * One Spring Batch job execution (BATCH_JOB_EXECUTION + step aggregates).
     * {@code posted}/{@code rejected} are POSTTRAN-only (from the step's
     * ExecutionContext); null for INTCALC.
     */
    public record JobRunView(
            long runId,
            String jobName,
            String status,
            String exitCode,
            LocalDateTime startTime,
            LocalDateTime endTime,
            long readCount,
            long writeCount,
            Integer posted,
            Integer rejected) {
    }
}
