package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.Cbstm03bRequest;
import com.client.core.dto.Cbstm03bResponse;
import com.client.core.service.Cbstm03bService;
import com.client.core.service.Cbstm03bService.ModernizedOperationResult;
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
@RequestMapping("/api/v1/core/cbstm03b")
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

    @PostMapping("/cbstm03b001")
    @Operation(summary = "Execute CBSTM03B Transaction Processing", description = "Endpoint invoking Cbstm03bService#executeCbstm03b001 (Rule BR-CBSTM03B-001)")
    public ResponseEntity<Cbstm03bResponse> handleCbstm03b001(@Valid @RequestBody(required = false) Cbstm03bRequest request) {
        log.info("REST POST /api/v1/core/cbstm03b/cbstm03b001 invoked");
        Cbstm03bResponse result = service.executeCbstm03b001(request);
        return ResponseEntity.ok(result);
    }
}
