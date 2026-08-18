package com.carddemo.api;

import com.carddemo.api.dto.ReportDtos.StatementView;
import org.springframework.core.io.FileSystemResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.nio.file.Path;
import java.util.List;

/**
 * Statement endpoints (plan §6): list per account, download txt or view html.
 * Backed by the CBSTM03A output — replaces the STMTFILE/HTMLFILE datasets.
 */
@RestController
@RequestMapping("/api/v1")
public class StatementController {

    private final StatementService statementService;

    public StatementController(StatementService statementService) {
        this.statementService = statementService;
    }

    @GetMapping("/accounts/{id}/statements")
    public List<StatementView> listForAccount(@PathVariable("id") long accountId) {
        return statementService.listForAccount(accountId);
    }

    /** txt → attachment (like the report download); html → inline view. */
    @GetMapping("/statements/{id}")
    public ResponseEntity<FileSystemResource> download(
            @PathVariable("id") long id,
            @RequestParam(name = "format", defaultValue = "txt") String format) {
        Path file = statementService.statementFile(id, format);
        boolean html = "html".equals(format);
        return ResponseEntity.ok()
                .contentType(html ? MediaType.TEXT_HTML : MediaType.TEXT_PLAIN)
                .header(HttpHeaders.CONTENT_DISPOSITION,
                        (html ? "inline" : "attachment") + "; filename=\"" + file.getFileName() + "\"")
                // stored-XSS mitigation: the statement HTML embeds unescaped
                // legacy text (parity — file bytes must not change), so serve
                // it fully sandboxed (no scripts, opaque origin)
                .header("Content-Security-Policy", "sandbox")
                .body(new FileSystemResource(file));
    }
}
