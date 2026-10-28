package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.Codate01Request;
import com.client.core.dto.Codate01Response;
import com.client.core.service.Codate01Service;
import com.client.core.service.Codate01Service.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: Codate01Controller
 *
 * RESTful HTTP boundary for modernized COBOL module: CODATE01
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/codate01")
@Tag(name = "Codate01Controller", description = "REST API Controller for CODATE01")
public class Codate01Controller {

    private final Codate01Service service;

    public Codate01Controller(Codate01Service service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies Codate01Service status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/codate01001")
    @Operation(summary = "Execute CODATE01 Transaction Processing", description = "Endpoint invoking Codate01Service#executeCodate01001 (Rule BR-CODATE01-001)")
    public ResponseEntity<Codate01Response> handleCodate01001(@Valid @RequestBody(required = false) Codate01Request request) {
        log.info("REST POST /api/v1/core/codate01/codate01001 invoked");
        Codate01Response result = service.executeCodate01001(request);
        return ResponseEntity.ok(result);
    }
}
