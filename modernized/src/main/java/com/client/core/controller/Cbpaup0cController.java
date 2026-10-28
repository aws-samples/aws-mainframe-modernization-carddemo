package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.Cbpaup0cRequest;
import com.client.core.dto.Cbpaup0cResponse;
import com.client.core.service.Cbpaup0cService;
import com.client.core.service.Cbpaup0cService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: Cbpaup0cController
 *
 * RESTful HTTP boundary for modernized COBOL module: CBPAUP0C
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/cbpaup0c")
@Tag(name = "Cbpaup0cController", description = "REST API Controller for CBPAUP0C")
public class Cbpaup0cController {

    private final Cbpaup0cService service;

    public Cbpaup0cController(Cbpaup0cService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies Cbpaup0cService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/cbpaup0c001")
    @Operation(summary = "Execute CBPAUP0C Transaction Processing", description = "Endpoint invoking Cbpaup0cService#executeCbpaup0c001 (Rule BR-CBPAUP0C-001)")
    public ResponseEntity<Cbpaup0cResponse> handleCbpaup0c001(@Valid @RequestBody(required = false) Cbpaup0cRequest request) {
        log.info("REST POST /api/v1/core/cbpaup0c/cbpaup0c001 invoked");
        Cbpaup0cResponse result = service.executeCbpaup0c001(request);
        return ResponseEntity.ok(result);
    }
}
