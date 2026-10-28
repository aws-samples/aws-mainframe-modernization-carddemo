package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.Comen01cRequest;
import com.client.core.dto.Comen01cResponse;
import com.client.core.service.Comen01cService;
import com.client.core.service.Comen01cService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: Comen01cController
 *
 * RESTful HTTP boundary for modernized COBOL module: COMEN01C
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/comen01c")
@Tag(name = "Comen01cController", description = "REST API Controller for COMEN01C")
public class Comen01cController {

    private final Comen01cService service;

    public Comen01cController(Comen01cService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies Comen01cService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/navigate-menu")
    @Operation(summary = "Navigate Menu", description = "Endpoint invoking Comen01cService#navigateMenu")
    public ResponseEntity<Comen01cResponse> handleNavigateMenu(@Valid @RequestBody(required = false) Comen01cRequest request) {
        return ResponseEntity.ok(service.navigateMenu(request));
    }

    @PostMapping("/comen01c001")
    @Operation(summary = "Execute COMEN01C Transaction Processing", description = "Endpoint invoking Comen01cService#executeComen01c001 (Rule BR-COMEN01C-001)")
    public ResponseEntity<Comen01cResponse> handleComen01c001(@Valid @RequestBody(required = false) Comen01cRequest request) {
        log.info("REST POST /api/v1/core/comen01c/comen01c001 invoked");
        Comen01cResponse result = service.executeComen01c001(request);
        return ResponseEntity.ok(result);
    }
}
