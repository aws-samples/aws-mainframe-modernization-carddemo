package com.carddemo.web;

import com.carddemo.service.JobService;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/** Batch-job endpoints (CBTRN01-03C, CBACT04C, CBSTM03A/B). */
@RestController
@RequestMapping("/api/jobs")
public class JobController {

    private final JobService service;

    public JobController(JobService service) {
        this.service = service;
    }

    @PostMapping("/posting")
    public Dto.PostingResult posting() {
        return service.runPostingJob();
    }

    @PostMapping("/interest")
    public Dto.InterestResult interest() {
        return service.runInterestJob();
    }

    @PostMapping("/statements")
    public List<Dto.StatementResult> statements() {
        return service.runStatementJob();
    }

    @GetMapping("/statements/{acctId}")
    public Dto.StatementResult statement(@PathVariable long acctId) {
        return service.statementFor(acctId);
    }
}
