package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.Coadm01cRequest;
import com.client.core.dto.Coadm01cResponse;
import com.client.core.service.Coadm01cService;
import com.client.core.service.Coadm01cService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: Coadm01cController
 *
 * RESTful HTTP boundary for modernized COBOL module: COADM01C
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/coadm01c")
@Tag(name = "Coadm01cController", description = "REST API Controller for COADM01C")
public class Coadm01cController {

    private final Coadm01cService service;

    public Coadm01cController(Coadm01cService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies Coadm01cService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/manage-user")
    @Operation(summary = "Manage Security User", description = "Endpoint invoking Coadm01cService#manageUser")
    public ResponseEntity<Coadm01cResponse> handleManageUser(@Valid @RequestBody(required = false) Coadm01cRequest request) {
        return ResponseEntity.ok(service.manageUser(request));
    }

    @PostMapping("/coadm01c001")
    @Operation(summary = "Execute COADM01C Transaction Processing", description = "Endpoint invoking Coadm01cService#executeCoadm01c001 (Rule BR-COADM01C-001)")
    public ResponseEntity<Coadm01cResponse> handleCoadm01c001(@Valid @RequestBody(required = false) Coadm01cRequest request) {
        log.info("REST POST /api/v1/core/coadm01c/coadm01c001 invoked");
        Coadm01cResponse result = service.executeCoadm01c001(request);
        return ResponseEntity.ok(result);
    }
}
