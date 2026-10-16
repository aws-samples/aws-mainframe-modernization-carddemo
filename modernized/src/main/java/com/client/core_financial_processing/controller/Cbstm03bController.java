package com.client.core_financial_processing.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core_financial_processing.dto.Cbstm03bRequest;
import com.client.core_financial_processing.dto.Cbstm03bResponse;
import com.client.core_financial_processing.service.Cbstm03bService;
import com.client.core_financial_processing.service.Cbstm03bService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: Cbstm03bController
 *
 * RESTful HTTP boundary for modernized COBOL module: CBSTM03B
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core_financial_processing/cbstm03b")
@Tag(name = "Cbstm03bController", description = "REST API Controller for CBSTM03B")
public class Cbstm03bController {

    private final Cbstm03bService service;

    public Cbstm03bController(Cbstm03bService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies Cbstm03bService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/generate-statement")
    @Operation(summary = "Generate Monthly Statement", description = "Endpoint invoking Cbstm03bService#generateStatement")
    public ResponseEntity<Cbstm03bResponse> handleGenerateStatement(@Valid @RequestBody(required = false) Cbstm03bRequest request) {
        return ResponseEntity.ok(service.generateStatement(request));
    }

    @PostMapping("/cbstm03b0163")
    @Operation(summary = "Business logic rule BR-CBSTM03B-0163", description = "Endpoint invoking Cbstm03bService#executeCbstm03b0163 (Rule BR-CBSTM03B-0163)")
    public ResponseEntity<Cbstm03bResponse> handleCbstm03b0163(@Valid @RequestBody(required = false) Cbstm03bRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbstm03b/cbstm03b0163 invoked");
        Cbstm03bResponse result = service.executeCbstm03b0163(request);
        return ResponseEntity.ok(result);
    }

    @PostMapping("/cbstm03b0164")
    @Operation(summary = "Business logic rule BR-CBSTM03B-0164", description = "Endpoint invoking Cbstm03bService#executeCbstm03b0164 (Rule BR-CBSTM03B-0164)")
    public ResponseEntity<Cbstm03bResponse> handleCbstm03b0164(@Valid @RequestBody(required = false) Cbstm03bRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbstm03b/cbstm03b0164 invoked");
        Cbstm03bResponse result = service.executeCbstm03b0164(request);
        return ResponseEntity.ok(result);
    }

    @PostMapping("/cbstm03b0165")
    @Operation(summary = "Business logic rule BR-CBSTM03B-0165", description = "Endpoint invoking Cbstm03bService#executeCbstm03b0165 (Rule BR-CBSTM03B-0165)")
    public ResponseEntity<Cbstm03bResponse> handleCbstm03b0165(@Valid @RequestBody(required = false) Cbstm03bRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbstm03b/cbstm03b0165 invoked");
        Cbstm03bResponse result = service.executeCbstm03b0165(request);
        return ResponseEntity.ok(result);
    }
}
