package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.Copaus0cRequest;
import com.client.core.dto.Copaus0cResponse;
import com.client.core.service.Copaus0cService;
import com.client.core.service.Copaus0cService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: Copaus0cController
 *
 * RESTful HTTP boundary for modernized COBOL module: COPAUS0C
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/copaus0c")
@Tag(name = "Copaus0cController", description = "REST API Controller for COPAUS0C")
public class Copaus0cController {

    private final Copaus0cService service;

    public Copaus0cController(Copaus0cService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies Copaus0cService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/copaus0c001")
    @Operation(summary = "Execute COPAUS0C Transaction Processing", description = "Endpoint invoking Copaus0cService#executeCopaus0c001 (Rule BR-COPAUS0C-001)")
    public ResponseEntity<Copaus0cResponse> handleCopaus0c001(@Valid @RequestBody(required = false) Copaus0cRequest request) {
        log.info("REST POST /api/v1/core/copaus0c/copaus0c001 invoked");
        Copaus0cResponse result = service.executeCopaus0c001(request);
        return ResponseEntity.ok(result);
    }
}
