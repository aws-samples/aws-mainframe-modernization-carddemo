package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.CobswaitRequest;
import com.client.core.dto.CobswaitResponse;
import com.client.core.service.CobswaitService;
import com.client.core.service.CobswaitService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: CobswaitController
 *
 * RESTful HTTP boundary for modernized COBOL module: COBSWAIT
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/cobswait")
@Tag(name = "CobswaitController", description = "REST API Controller for COBSWAIT")
public class CobswaitController {

    private final CobswaitService service;

    public CobswaitController(CobswaitService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies CobswaitService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/execute-wait")
    @Operation(summary = "Execute Centisecond Delay", description = "Endpoint invoking CobswaitService#executeWait")
    public ResponseEntity<CobswaitResponse> handleExecuteWait(@Valid @RequestBody(required = false) CobswaitRequest request) {
        return ResponseEntity.ok(service.executeWait(request));
    }

    @PostMapping("/cobswait001")
    @Operation(summary = "Execute COBSWAIT Transaction Processing", description = "Endpoint invoking CobswaitService#executeCobswait001 (Rule BR-COBSWAIT-001)")
    public ResponseEntity<CobswaitResponse> handleCobswait001(@Valid @RequestBody(required = false) CobswaitRequest request) {
        log.info("REST POST /api/v1/core/cobswait/cobswait001 invoked");
        CobswaitResponse result = service.executeCobswait001(request);
        return ResponseEntity.ok(result);
    }
}
