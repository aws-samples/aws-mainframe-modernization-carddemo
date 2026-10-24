package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.GeneratedServiceTargetCbexportbatchjobconfigRequest;
import com.client.core.dto.GeneratedServiceTargetCbexportbatchjobconfigResponse;
import com.client.core.service.GeneratedServiceTargetCbexportbatchjobconfigService;
import com.client.core.service.GeneratedServiceTargetCbexportbatchjobconfigService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: GeneratedServiceTargetCbexportbatchjobconfigController
 *
 * RESTful HTTP boundary for modernized COBOL module: generated_service:TARGET_CBEXPORTBATCHJOBCONFIG
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/generated_service_target_cbexportbatchjobconfig")
@Tag(name = "GeneratedServiceTargetCbexportbatchjobconfigController", description = "REST API Controller for generated_service:TARGET_CBEXPORTBATCHJOBCONFIG")
public class GeneratedServiceTargetCbexportbatchjobconfigController {

    private final GeneratedServiceTargetCbexportbatchjobconfigService service;

    public GeneratedServiceTargetCbexportbatchjobconfigController(GeneratedServiceTargetCbexportbatchjobconfigService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies GeneratedServiceTargetCbexportbatchjobconfigService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/generatedservicetargetcbexportbatchjobconfig001")
    @Operation(summary = "Execute generated_service:TARGET_CBEXPORTBATCHJOBCONFIG Transaction Processing", description = "Endpoint invoking GeneratedServiceTargetCbexportbatchjobconfigService#executeGeneratedServiceTargetCbexportbatchjobconfig001 (Rule BR-GENERATED_SERVICE:TARGET_CBEXPORTBATCHJOBCONFIG-001)")
    public ResponseEntity<GeneratedServiceTargetCbexportbatchjobconfigResponse> handleGeneratedServiceTargetCbexportbatchjobconfig001(@Valid @RequestBody(required = false) GeneratedServiceTargetCbexportbatchjobconfigRequest request) {
        log.info("REST POST /api/v1/core/generated_service_target_cbexportbatchjobconfig/generatedservicetargetcbexportbatchjobconfig001 invoked");
        GeneratedServiceTargetCbexportbatchjobconfigResponse result = service.executeGeneratedServiceTargetCbexportbatchjobconfig001(request);
        return ResponseEntity.ok(result);
    }
}
