package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.Coacct01Request;
import com.client.core.dto.Coacct01Response;
import com.client.core.service.Coacct01Service;
import com.client.core.service.Coacct01Service.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: Coacct01Controller
 *
 * RESTful HTTP boundary for modernized COBOL module: COACCT01
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/coacct01")
@Tag(name = "Coacct01Controller", description = "REST API Controller for COACCT01")
public class Coacct01Controller {

    private final Coacct01Service service;

    public Coacct01Controller(Coacct01Service service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies Coacct01Service status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/coacct01001")
    @Operation(summary = "Execute COACCT01 Transaction Processing", description = "Endpoint invoking Coacct01Service#executeCoacct01001 (Rule BR-COACCT01-001)")
    public ResponseEntity<Coacct01Response> handleCoacct01001(@Valid @RequestBody(required = false) Coacct01Request request) {
        log.info("REST POST /api/v1/core/coacct01/coacct01001 invoked");
        Coacct01Response result = service.executeCoacct01001(request);
        return ResponseEntity.ok(result);
    }
}
