package com.client.core_financial_processing.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core_financial_processing.dto.Cbstm03aRequest;
import com.client.core_financial_processing.dto.Cbstm03aResponse;
import com.client.core_financial_processing.service.Cbstm03aService;
import com.client.core_financial_processing.service.Cbstm03aService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: Cbstm03aController
 *
 * RESTful HTTP boundary for modernized COBOL module: CBSTM03A
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core_financial_processing/cbstm03a")
@Tag(name = "Cbstm03aController", description = "REST API Controller for CBSTM03A")
public class Cbstm03aController {

    private final Cbstm03aService service;

    public Cbstm03aController(Cbstm03aService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies Cbstm03aService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/generate-statement")
    @Operation(summary = "Generate Monthly Statement", description = "Endpoint invoking Cbstm03aService#generateStatement")
    public ResponseEntity<Cbstm03aResponse> handleGenerateStatement(@Valid @RequestBody(required = false) Cbstm03aRequest request) {
        return ResponseEntity.ok(service.generateStatement(request));
    }

    @PostMapping("/cbstm03a0043")
    @Operation(summary = "Business logic rule BR-CBSTM03A-0043", description = "Endpoint invoking Cbstm03aService#executeCbstm03a0043 (Rule BR-CBSTM03A-0043)")
    public ResponseEntity<Cbstm03aResponse> handleCbstm03a0043(@Valid @RequestBody(required = false) Cbstm03aRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbstm03a/cbstm03a0043 invoked");
        Cbstm03aResponse result = service.executeCbstm03a0043(request);
        return ResponseEntity.ok(result);
    }

    @PostMapping("/cbstm03a0044")
    @Operation(summary = "Business logic rule BR-CBSTM03A-0044", description = "Endpoint invoking Cbstm03aService#executeCbstm03a0044 (Rule BR-CBSTM03A-0044)")
    public ResponseEntity<Cbstm03aResponse> handleCbstm03a0044(@Valid @RequestBody(required = false) Cbstm03aRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbstm03a/cbstm03a0044 invoked");
        Cbstm03aResponse result = service.executeCbstm03a0044(request);
        return ResponseEntity.ok(result);
    }

    @PostMapping("/cbstm03a0045")
    @Operation(summary = "Business logic rule BR-CBSTM03A-0045", description = "Endpoint invoking Cbstm03aService#executeCbstm03a0045 (Rule BR-CBSTM03A-0045)")
    public ResponseEntity<Cbstm03aResponse> handleCbstm03a0045(@Valid @RequestBody(required = false) Cbstm03aRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbstm03a/cbstm03a0045 invoked");
        Cbstm03aResponse result = service.executeCbstm03a0045(request);
        return ResponseEntity.ok(result);
    }

    @PostMapping("/cbstm03a0046")
    @Operation(summary = "Business logic rule BR-CBSTM03A-0046", description = "Endpoint invoking Cbstm03aService#executeCbstm03a0046 (Rule BR-CBSTM03A-0046)")
    public ResponseEntity<Cbstm03aResponse> handleCbstm03a0046(@Valid @RequestBody(required = false) Cbstm03aRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbstm03a/cbstm03a0046 invoked");
        Cbstm03aResponse result = service.executeCbstm03a0046(request);
        return ResponseEntity.ok(result);
    }

    @PostMapping("/cbstm03a0047")
    @Operation(summary = "Business logic rule BR-CBSTM03A-0047", description = "Endpoint invoking Cbstm03aService#executeCbstm03a0047 (Rule BR-CBSTM03A-0047)")
    public ResponseEntity<Cbstm03aResponse> handleCbstm03a0047(@Valid @RequestBody(required = false) Cbstm03aRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbstm03a/cbstm03a0047 invoked");
        Cbstm03aResponse result = service.executeCbstm03a0047(request);
        return ResponseEntity.ok(result);
    }

    @PostMapping("/cbstm03a0048")
    @Operation(summary = "Business logic rule BR-CBSTM03A-0048", description = "Endpoint invoking Cbstm03aService#executeCbstm03a0048 (Rule BR-CBSTM03A-0048)")
    public ResponseEntity<Cbstm03aResponse> handleCbstm03a0048(@Valid @RequestBody(required = false) Cbstm03aRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbstm03a/cbstm03a0048 invoked");
        Cbstm03aResponse result = service.executeCbstm03a0048(request);
        return ResponseEntity.ok(result);
    }

    @PostMapping("/cbstm03a0049")
    @Operation(summary = "Business logic rule BR-CBSTM03A-0049", description = "Endpoint invoking Cbstm03aService#executeCbstm03a0049 (Rule BR-CBSTM03A-0049)")
    public ResponseEntity<Cbstm03aResponse> handleCbstm03a0049(@Valid @RequestBody(required = false) Cbstm03aRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbstm03a/cbstm03a0049 invoked");
        Cbstm03aResponse result = service.executeCbstm03a0049(request);
        return ResponseEntity.ok(result);
    }

    @PostMapping("/cbstm03a0050")
    @Operation(summary = "Business logic rule BR-CBSTM03A-0050", description = "Endpoint invoking Cbstm03aService#executeCbstm03a0050 (Rule BR-CBSTM03A-0050)")
    public ResponseEntity<Cbstm03aResponse> handleCbstm03a0050(@Valid @RequestBody(required = false) Cbstm03aRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbstm03a/cbstm03a0050 invoked");
        Cbstm03aResponse result = service.executeCbstm03a0050(request);
        return ResponseEntity.ok(result);
    }
}
