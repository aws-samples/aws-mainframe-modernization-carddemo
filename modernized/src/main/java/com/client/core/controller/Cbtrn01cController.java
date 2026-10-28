package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.Cbtrn01cRequest;
import com.client.core.dto.Cbtrn01cResponse;
import com.client.core.service.Cbtrn01cService;
import com.client.core.service.Cbtrn01cService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: Cbtrn01cController
 *
 * RESTful HTTP boundary for modernized COBOL module: CBTRN01C
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/cbtrn01c")
@Tag(name = "Cbtrn01cController", description = "REST API Controller for CBTRN01C")
public class Cbtrn01cController {

    private final Cbtrn01cService service;

    public Cbtrn01cController(Cbtrn01cService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies Cbtrn01cService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/cbtrn01c001")
    @Operation(summary = "Execute CBTRN01C Transaction Processing", description = "Endpoint invoking Cbtrn01cService#executeCbtrn01c001 (Rule BR-CBTRN01C-001)")
    public ResponseEntity<Cbtrn01cResponse> handleCbtrn01c001(@Valid @RequestBody(required = false) Cbtrn01cRequest request) {
        log.info("REST POST /api/v1/core/cbtrn01c/cbtrn01c001 invoked");
        Cbtrn01cResponse result = service.executeCbtrn01c001(request);
        return ResponseEntity.ok(result);
    }
}
