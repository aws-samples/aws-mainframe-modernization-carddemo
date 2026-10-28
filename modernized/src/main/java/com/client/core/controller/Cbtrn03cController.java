package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.Cbtrn03cRequest;
import com.client.core.dto.Cbtrn03cResponse;
import com.client.core.service.Cbtrn03cService;
import com.client.core.service.Cbtrn03cService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: Cbtrn03cController
 *
 * RESTful HTTP boundary for modernized COBOL module: CBTRN03C
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/cbtrn03c")
@Tag(name = "Cbtrn03cController", description = "REST API Controller for CBTRN03C")
public class Cbtrn03cController {

    private final Cbtrn03cService service;

    public Cbtrn03cController(Cbtrn03cService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies Cbtrn03cService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/process-batch")
    @Operation(summary = "Process Batch File", description = "Endpoint invoking Cbtrn03cService#processBatchTransactions")
    public ResponseEntity<Cbtrn03cResponse> handleProcessBatch(@Valid @RequestBody(required = false) Cbtrn03cRequest request) {
        return ResponseEntity.ok(service.processBatchTransactions(request));
    }

    @PostMapping("/cbtrn03c001")
    @Operation(summary = "Execute CBTRN03C Transaction Processing", description = "Endpoint invoking Cbtrn03cService#executeCbtrn03c001 (Rule BR-CBTRN03C-001)")
    public ResponseEntity<Cbtrn03cResponse> handleCbtrn03c001(@Valid @RequestBody(required = false) Cbtrn03cRequest request) {
        log.info("REST POST /api/v1/core/cbtrn03c/cbtrn03c001 invoked");
        Cbtrn03cResponse result = service.executeCbtrn03c001(request);
        return ResponseEntity.ok(result);
    }
}
