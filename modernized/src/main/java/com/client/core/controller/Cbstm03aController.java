package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.Cbstm03aRequest;
import com.client.core.dto.Cbstm03aResponse;
import com.client.core.service.Cbstm03aService;
import com.client.core.service.Cbstm03aService.ModernizedOperationResult;
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
@RequestMapping("/api/v1/core/cbstm03a")
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

    @PostMapping("/cbstm03a001")
    @Operation(summary = "Execute CBSTM03A Transaction Processing", description = "Endpoint invoking Cbstm03aService#executeCbstm03a001 (Rule BR-CBSTM03A-001)")
    public ResponseEntity<Cbstm03aResponse> handleCbstm03a001(@Valid @RequestBody(required = false) Cbstm03aRequest request) {
        log.info("REST POST /api/v1/core/cbstm03a/cbstm03a001 invoked");
        Cbstm03aResponse result = service.executeCbstm03a001(request);
        return ResponseEntity.ok(result);
    }
}
