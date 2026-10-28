package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.CobtupdtRequest;
import com.client.core.dto.CobtupdtResponse;
import com.client.core.service.CobtupdtService;
import com.client.core.service.CobtupdtService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: CobtupdtController
 *
 * RESTful HTTP boundary for modernized COBOL module: COBTUPDT
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/cobtupdt")
@Tag(name = "CobtupdtController", description = "REST API Controller for COBTUPDT")
public class CobtupdtController {

    private final CobtupdtService service;

    public CobtupdtController(CobtupdtService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies CobtupdtService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/cobtupdt001")
    @Operation(summary = "Execute COBTUPDT Transaction Processing", description = "Endpoint invoking CobtupdtService#executeCobtupdt001 (Rule BR-COBTUPDT-001)")
    public ResponseEntity<CobtupdtResponse> handleCobtupdt001(@Valid @RequestBody(required = false) CobtupdtRequest request) {
        log.info("REST POST /api/v1/core/cobtupdt/cobtupdt001 invoked");
        CobtupdtResponse result = service.executeCobtupdt001(request);
        return ResponseEntity.ok(result);
    }
}
