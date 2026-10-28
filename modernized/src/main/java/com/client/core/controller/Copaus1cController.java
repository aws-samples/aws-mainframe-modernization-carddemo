package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.Copaus1cRequest;
import com.client.core.dto.Copaus1cResponse;
import com.client.core.service.Copaus1cService;
import com.client.core.service.Copaus1cService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: Copaus1cController
 *
 * RESTful HTTP boundary for modernized COBOL module: COPAUS1C
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/copaus1c")
@Tag(name = "Copaus1cController", description = "REST API Controller for COPAUS1C")
public class Copaus1cController {

    private final Copaus1cService service;

    public Copaus1cController(Copaus1cService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies Copaus1cService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/copaus1c001")
    @Operation(summary = "Execute COPAUS1C Transaction Processing", description = "Endpoint invoking Copaus1cService#executeCopaus1c001 (Rule BR-COPAUS1C-001)")
    public ResponseEntity<Copaus1cResponse> handleCopaus1c001(@Valid @RequestBody(required = false) Copaus1cRequest request) {
        log.info("REST POST /api/v1/core/copaus1c/copaus1c001 invoked");
        Copaus1cResponse result = service.executeCopaus1c001(request);
        return ResponseEntity.ok(result);
    }
}
