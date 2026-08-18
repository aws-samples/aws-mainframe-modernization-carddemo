package com.carddemo.api;

import com.carddemo.api.dto.ReportDtos.ReportRequest;
import com.carddemo.api.dto.ReportDtos.ReportRunView;
import org.springframework.core.io.FileSystemResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import java.nio.file.Path;
import java.util.List;

/**
 * /api/v1/reports — CORPT00C + the CBTRN03C SYSOUT replacement (plan §6):
 * request a transaction report, list past runs (report_runs), download the
 * report file. Any authenticated user (the legacy menu exposes CORPT00 to
 * usrtype 'U').
 */
@RestController
@RequestMapping("/api/v1/reports")
public class ReportController {

    private final ReportService reportService;

    public ReportController(ReportService reportService) {
        this.reportService = reportService;
    }

    /** Requests + runs a report; requested_by = the JWT subject (user id). */
    @PostMapping("/transaction")
    @ResponseStatus(HttpStatus.CREATED)
    public ReportRunView request(@RequestBody ReportRequest request,
                                 @AuthenticationPrincipal Jwt jwt) {
        return reportService.requestReport(request, jwt.getSubject());
    }

    @GetMapping
    public List<ReportRunView> list() {
        return reportService.listRuns();
    }

    /** The report file as a text/plain attachment; 409 while not completed. */
    @GetMapping("/{id}/download")
    public ResponseEntity<FileSystemResource> download(@PathVariable("id") long id) {
        Path file = reportService.reportFile(id);
        return ResponseEntity.ok()
                .contentType(MediaType.TEXT_PLAIN)
                .header(HttpHeaders.CONTENT_DISPOSITION,
                        "attachment; filename=\"" + file.getFileName() + "\"")
                .body(new FileSystemResource(file));
    }
}
