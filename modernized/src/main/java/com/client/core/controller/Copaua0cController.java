package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.Copaua0cRequest;
import com.client.core.dto.Copaua0cResponse;
import com.client.core.service.Copaua0cService;
import com.client.core.service.Copaua0cService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: Copaua0cController
 *
 * RESTful HTTP boundary for modernized COBOL module: COPAUA0C
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/copaua0c")
@Tag(name = "Copaua0cController", description = "REST API Controller for COPAUA0C")
public class Copaua0cController {

    private final Copaua0cService service;

    public Copaua0cController(Copaua0cService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies Copaua0cService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/authorize-payment")
    @Operation(summary = "Authorize Payment", description = "Endpoint invoking Copaua0cService#authorizePayment")
    public ResponseEntity<Copaua0cResponse> handleAuthorizePayment(@Valid @RequestBody(required = false) Copaua0cRequest request) {
        return ResponseEntity.ok(service.authorizePayment(request));
    }

    @PostMapping("/copaua0c001")
    @Operation(summary = "Execute COPAUA0C Transaction Processing", description = "Endpoint invoking Copaua0cService#executeCopaua0c001 (Rule BR-COPAUA0C-001)")
    public ResponseEntity<Copaua0cResponse> handleCopaua0c001(@Valid @RequestBody(required = false) Copaua0cRequest request) {
        log.info("REST POST /api/v1/core/copaua0c/copaua0c001 invoked");
        Copaua0cResponse result = service.executeCopaua0c001(request);
        return ResponseEntity.ok(result);
    }
}
