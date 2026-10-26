package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.GeneratedServiceTargetCbpaup0cbatchjobconfigRequest;
import com.client.core.dto.GeneratedServiceTargetCbpaup0cbatchjobconfigResponse;
import com.client.core.service.GeneratedServiceTargetCbpaup0cbatchjobconfigService;
import com.client.core.service.GeneratedServiceTargetCbpaup0cbatchjobconfigService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: GeneratedServiceTargetCbpaup0cbatchjobconfigController
 *
 * RESTful HTTP boundary for modernized COBOL module: generated_service:TARGET_CBPAUP0CBATCHJOBCONFIG
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/generated_service_target_cbpaup0cbatchjobconfig")
@Tag(name = "GeneratedServiceTargetCbpaup0cbatchjobconfigController", description = "REST API Controller for generated_service:TARGET_CBPAUP0CBATCHJOBCONFIG")
public class GeneratedServiceTargetCbpaup0cbatchjobconfigController {

    private final GeneratedServiceTargetCbpaup0cbatchjobconfigService service;

    public GeneratedServiceTargetCbpaup0cbatchjobconfigController(GeneratedServiceTargetCbpaup0cbatchjobconfigService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies GeneratedServiceTargetCbpaup0cbatchjobconfigService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/generatedservicetargetcbpaup0cbatchjobconfig001")
    @Operation(summary = "Execute generated_service:TARGET_CBPAUP0CBATCHJOBCONFIG Transaction Processing", description = "Endpoint invoking GeneratedServiceTargetCbpaup0cbatchjobconfigService#executeGeneratedServiceTargetCbpaup0cbatchjobconfig001 (Rule BR-GENERATED_SERVICE:TARGET_CBPAUP0CBATCHJOBCONFIG-001)")
    public ResponseEntity<GeneratedServiceTargetCbpaup0cbatchjobconfigResponse> handleGeneratedServiceTargetCbpaup0cbatchjobconfig001(@Valid @RequestBody(required = false) GeneratedServiceTargetCbpaup0cbatchjobconfigRequest request) {
        log.info("REST POST /api/v1/core/generated_service_target_cbpaup0cbatchjobconfig/generatedservicetargetcbpaup0cbatchjobconfig001 invoked");
        GeneratedServiceTargetCbpaup0cbatchjobconfigResponse result = service.executeGeneratedServiceTargetCbpaup0cbatchjobconfig001(request);
        return ResponseEntity.ok(result);
    }
}
