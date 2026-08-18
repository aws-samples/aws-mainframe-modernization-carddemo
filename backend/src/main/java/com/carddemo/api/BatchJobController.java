package com.carddemo.api;

import com.carddemo.api.dto.BatchJobDtos.JobRunView;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * /api/v1/admin/jobs — batch triggers + run history (plan §6). ADMIN-only via
 * the SecurityConfig {@code /api/v1/admin/**} rule. Replaces the legacy JCL
 * submits for POSTTRAN (CBTRN02C) and INTCALC (CBACT04C).
 */
@RestController
@RequestMapping("/api/v1/admin/jobs")
public class BatchJobController {

    private final BatchJobService batchJobService;

    public BatchJobController(BatchJobService batchJobService) {
        this.batchJobService = batchJobService;
    }

    /** Runs the named job synchronously; 404 for unknown names. */
    @PostMapping("/{jobName}/run")
    @ResponseStatus(HttpStatus.CREATED)
    public JobRunView run(@PathVariable("jobName") String jobName) {
        return batchJobService.run(jobName);
    }

    @GetMapping("/runs")
    public List<JobRunView> listRuns() {
        return batchJobService.listRuns();
    }
}
