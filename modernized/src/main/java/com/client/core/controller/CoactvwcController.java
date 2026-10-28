package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.CoactvwcRequest;
import com.client.core.dto.CoactvwcResponse;
import com.client.core.service.CoactvwcService;
import com.client.core.service.CoactvwcService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: CoactvwcController
 *
 * RESTful HTTP boundary for modernized COBOL module: COACTVWC
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/coactvwc")
@Tag(name = "CoactvwcController", description = "REST API Controller for COACTVWC")
public class CoactvwcController {

    private final CoactvwcService service;

    public CoactvwcController(CoactvwcService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies CoactvwcService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/process-customer")
    @Operation(summary = "Lookup Customer Demographics", description = "Endpoint invoking CoactvwcService#processCustomer")
    public ResponseEntity<CoactvwcResponse> handleProcessCustomer(@Valid @RequestBody(required = false) CoactvwcRequest request) {
        return ResponseEntity.ok(service.processCustomer(request));
    }

    @PostMapping("/coactvwc001")
    @Operation(summary = "Execute COACTVWC Transaction Processing", description = "Endpoint invoking CoactvwcService#executeCoactvwc001 (Rule BR-COACTVWC-001)")
    public ResponseEntity<CoactvwcResponse> handleCoactvwc001(@Valid @RequestBody(required = false) CoactvwcRequest request) {
        log.info("REST POST /api/v1/core/coactvwc/coactvwc001 invoked");
        CoactvwcResponse result = service.executeCoactvwc001(request);
        return ResponseEntity.ok(result);
    }
}
