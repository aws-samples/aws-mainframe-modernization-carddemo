package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.Copaus2cRequest;
import com.client.core.dto.Copaus2cResponse;
import com.client.core.service.Copaus2cService;
import com.client.core.service.Copaus2cService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: Copaus2cController
 *
 * RESTful HTTP boundary for modernized COBOL module: COPAUS2C
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/copaus2c")
@Tag(name = "Copaus2cController", description = "REST API Controller for COPAUS2C")
public class Copaus2cController {

    private final Copaus2cService service;

    public Copaus2cController(Copaus2cService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies Copaus2cService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/copaus2c001")
    @Operation(summary = "Execute COPAUS2C Transaction Processing", description = "Endpoint invoking Copaus2cService#executeCopaus2c001 (Rule BR-COPAUS2C-001)")
    public ResponseEntity<Copaus2cResponse> handleCopaus2c001(@Valid @RequestBody(required = false) Copaus2cRequest request) {
        log.info("REST POST /api/v1/core/copaus2c/copaus2c001 invoked");
        Copaus2cResponse result = service.executeCopaus2c001(request);
        return ResponseEntity.ok(result);
    }
}
