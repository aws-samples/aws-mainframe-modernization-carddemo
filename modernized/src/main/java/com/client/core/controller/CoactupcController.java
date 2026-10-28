package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.CoactupcRequest;
import com.client.core.dto.CoactupcResponse;
import com.client.core.service.CoactupcService;
import com.client.core.service.CoactupcService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: CoactupcController
 *
 * RESTful HTTP boundary for modernized COBOL module: COACTUPC
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/coactupc")
@Tag(name = "CoactupcController", description = "REST API Controller for COACTUPC")
public class CoactupcController {

    private final CoactupcService service;

    public CoactupcController(CoactupcService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies CoactupcService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/process-customer")
    @Operation(summary = "Lookup Customer Demographics", description = "Endpoint invoking CoactupcService#processCustomer")
    public ResponseEntity<CoactupcResponse> handleProcessCustomer(@Valid @RequestBody(required = false) CoactupcRequest request) {
        return ResponseEntity.ok(service.processCustomer(request));
    }

    @PostMapping("/coactupc001")
    @Operation(summary = "Execute COACTUPC Transaction Processing", description = "Endpoint invoking CoactupcService#executeCoactupc001 (Rule BR-COACTUPC-001)")
    public ResponseEntity<CoactupcResponse> handleCoactupc001(@Valid @RequestBody(required = false) CoactupcRequest request) {
        log.info("REST POST /api/v1/core/coactupc/coactupc001 invoked");
        CoactupcResponse result = service.executeCoactupc001(request);
        return ResponseEntity.ok(result);
    }
}
