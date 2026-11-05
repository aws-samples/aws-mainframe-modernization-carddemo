package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.GeneratedServiceTargetCbimportbatchjobconfigRequest;
import com.client.core.dto.GeneratedServiceTargetCbimportbatchjobconfigResponse;
import com.client.core.service.GeneratedServiceTargetCbimportbatchjobconfigService;
import com.client.core.service.GeneratedServiceTargetCbimportbatchjobconfigService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: GeneratedServiceTargetCbimportbatchjobconfigController
 *
 * RESTful HTTP boundary for modernized COBOL module: generated_service:TARGET_CBIMPORTBATCHJOBCONFIG
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/generated_service_target_cbimportbatchjobconfig")
@Tag(name = "GeneratedServiceTargetCbimportbatchjobconfigController", description = "REST API Controller for generated_service:TARGET_CBIMPORTBATCHJOBCONFIG")
public class GeneratedServiceTargetCbimportbatchjobconfigController {

    private final GeneratedServiceTargetCbimportbatchjobconfigService service;

    public GeneratedServiceTargetCbimportbatchjobconfigController(GeneratedServiceTargetCbimportbatchjobconfigService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies GeneratedServiceTargetCbimportbatchjobconfigService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/generatedservicetargetcbimportbatchjobconfig001")
    @Operation(summary = "Execute generated_service:TARGET_CBIMPORTBATCHJOBCONFIG Transaction Processing", description = "Endpoint invoking GeneratedServiceTargetCbimportbatchjobconfigService#executeGeneratedServiceTargetCbimportbatchjobconfig001 (Rule BR-GENERATED_SERVICE:TARGET_CBIMPORTBATCHJOBCONFIG-001)")
    public ResponseEntity<GeneratedServiceTargetCbimportbatchjobconfigResponse> handleGeneratedServiceTargetCbimportbatchjobconfig001(@Valid @RequestBody(required = false) GeneratedServiceTargetCbimportbatchjobconfigRequest request) {
        log.info("REST POST /api/v1/core/generated_service_target_cbimportbatchjobconfig/generatedservicetargetcbimportbatchjobconfig001 invoked");
        GeneratedServiceTargetCbimportbatchjobconfigResponse result = service.executeGeneratedServiceTargetCbimportbatchjobconfig001(request);
        return ResponseEntity.ok(result);
    }
}
