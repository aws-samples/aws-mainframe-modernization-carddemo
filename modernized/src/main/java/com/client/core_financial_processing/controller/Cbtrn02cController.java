package com.client.core_financial_processing.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core_financial_processing.dto.Cbtrn02cRequest;
import com.client.core_financial_processing.dto.Cbtrn02cResponse;
import com.client.core_financial_processing.service.Cbtrn02cService;
import com.client.core_financial_processing.service.Cbtrn02cService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: Cbtrn02cController
 *
 * RESTful HTTP boundary for modernized COBOL module: CBTRN02C
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core_financial_processing/cbtrn02c")
@Tag(name = "Cbtrn02cController", description = "REST API Controller for CBTRN02C")
public class Cbtrn02cController {

    private final Cbtrn02cService service;

    public Cbtrn02cController(Cbtrn02cService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies Cbtrn02cService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/process-transaction")
    @Operation(summary = "Post CardDemo Transaction", description = "Endpoint invoking Cbtrn02cService#processTransaction")
    public ResponseEntity<Cbtrn02cResponse> handleProcessTransaction(@Valid @RequestBody(required = false) Cbtrn02cRequest request) {
        return ResponseEntity.ok(service.processTransaction(request));
    }

    @PostMapping("/cbtrn02c0051")
    @Operation(summary = "Business logic rule BR-CBTRN02C-0051", description = "Endpoint invoking Cbtrn02cService#executeCbtrn02c0051 (Rule BR-CBTRN02C-0051)")
    public ResponseEntity<Cbtrn02cResponse> handleCbtrn02c0051(@Valid @RequestBody(required = false) Cbtrn02cRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbtrn02c/cbtrn02c0051 invoked");
        Cbtrn02cResponse result = service.executeCbtrn02c0051(request);
        return ResponseEntity.ok(result);
    }

    @PostMapping("/cbtrn02c0052")
    @Operation(summary = "Business logic rule BR-CBTRN02C-0052", description = "Endpoint invoking Cbtrn02cService#executeCbtrn02c0052 (Rule BR-CBTRN02C-0052)")
    public ResponseEntity<Cbtrn02cResponse> handleCbtrn02c0052(@Valid @RequestBody(required = false) Cbtrn02cRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbtrn02c/cbtrn02c0052 invoked");
        Cbtrn02cResponse result = service.executeCbtrn02c0052(request);
        return ResponseEntity.ok(result);
    }

    @PostMapping("/cbtrn02c0053")
    @Operation(summary = "Business logic rule BR-CBTRN02C-0053", description = "Endpoint invoking Cbtrn02cService#executeCbtrn02c0053 (Rule BR-CBTRN02C-0053)")
    public ResponseEntity<Cbtrn02cResponse> handleCbtrn02c0053(@Valid @RequestBody(required = false) Cbtrn02cRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbtrn02c/cbtrn02c0053 invoked");
        Cbtrn02cResponse result = service.executeCbtrn02c0053(request);
        return ResponseEntity.ok(result);
    }

    @PostMapping("/cbtrn02c0054")
    @Operation(summary = "Business logic rule BR-CBTRN02C-0054", description = "Endpoint invoking Cbtrn02cService#executeCbtrn02c0054 (Rule BR-CBTRN02C-0054)")
    public ResponseEntity<Cbtrn02cResponse> handleCbtrn02c0054(@Valid @RequestBody(required = false) Cbtrn02cRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbtrn02c/cbtrn02c0054 invoked");
        Cbtrn02cResponse result = service.executeCbtrn02c0054(request);
        return ResponseEntity.ok(result);
    }

    @PostMapping("/cbtrn02c0055")
    @Operation(summary = "Business logic rule BR-CBTRN02C-0055", description = "Endpoint invoking Cbtrn02cService#executeCbtrn02c0055 (Rule BR-CBTRN02C-0055)")
    public ResponseEntity<Cbtrn02cResponse> handleCbtrn02c0055(@Valid @RequestBody(required = false) Cbtrn02cRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbtrn02c/cbtrn02c0055 invoked");
        Cbtrn02cResponse result = service.executeCbtrn02c0055(request);
        return ResponseEntity.ok(result);
    }

    @PostMapping("/cbtrn02c0056")
    @Operation(summary = "Business logic rule BR-CBTRN02C-0056", description = "Endpoint invoking Cbtrn02cService#executeCbtrn02c0056 (Rule BR-CBTRN02C-0056)")
    public ResponseEntity<Cbtrn02cResponse> handleCbtrn02c0056(@Valid @RequestBody(required = false) Cbtrn02cRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbtrn02c/cbtrn02c0056 invoked");
        Cbtrn02cResponse result = service.executeCbtrn02c0056(request);
        return ResponseEntity.ok(result);
    }

    @PostMapping("/cbtrn02c0057")
    @Operation(summary = "Business logic rule BR-CBTRN02C-0057", description = "Endpoint invoking Cbtrn02cService#executeCbtrn02c0057 (Rule BR-CBTRN02C-0057)")
    public ResponseEntity<Cbtrn02cResponse> handleCbtrn02c0057(@Valid @RequestBody(required = false) Cbtrn02cRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbtrn02c/cbtrn02c0057 invoked");
        Cbtrn02cResponse result = service.executeCbtrn02c0057(request);
        return ResponseEntity.ok(result);
    }

    @PostMapping("/cbtrn02c0058")
    @Operation(summary = "Business logic rule BR-CBTRN02C-0058", description = "Endpoint invoking Cbtrn02cService#executeCbtrn02c0058 (Rule BR-CBTRN02C-0058)")
    public ResponseEntity<Cbtrn02cResponse> handleCbtrn02c0058(@Valid @RequestBody(required = false) Cbtrn02cRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbtrn02c/cbtrn02c0058 invoked");
        Cbtrn02cResponse result = service.executeCbtrn02c0058(request);
        return ResponseEntity.ok(result);
    }
}
