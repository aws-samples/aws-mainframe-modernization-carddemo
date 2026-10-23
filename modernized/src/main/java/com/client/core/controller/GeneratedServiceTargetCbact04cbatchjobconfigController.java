package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.GeneratedServiceTargetCbact04cbatchjobconfigRequest;
import com.client.core.dto.GeneratedServiceTargetCbact04cbatchjobconfigResponse;
import com.client.core.service.GeneratedServiceTargetCbact04cbatchjobconfigService;
import com.client.core.service.GeneratedServiceTargetCbact04cbatchjobconfigService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: GeneratedServiceTargetCbact04cbatchjobconfigController
 *
 * RESTful HTTP boundary for modernized COBOL module: generated_service:TARGET_CBACT04CBATCHJOBCONFIG
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/generated_service_target_cbact04cbatchjobconfig")
@Tag(name = "GeneratedServiceTargetCbact04cbatchjobconfigController", description = "REST API Controller for generated_service:TARGET_CBACT04CBATCHJOBCONFIG")
public class GeneratedServiceTargetCbact04cbatchjobconfigController {

    private final GeneratedServiceTargetCbact04cbatchjobconfigService service;

    public GeneratedServiceTargetCbact04cbatchjobconfigController(GeneratedServiceTargetCbact04cbatchjobconfigService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies GeneratedServiceTargetCbact04cbatchjobconfigService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/generatedservicetargetcbact04cbatchjobconfig001")
    @Operation(summary = "Execute generated_service:TARGET_CBACT04CBATCHJOBCONFIG Transaction Processing", description = "Endpoint invoking GeneratedServiceTargetCbact04cbatchjobconfigService#executeGeneratedServiceTargetCbact04cbatchjobconfig001 (Rule BR-GENERATED_SERVICE:TARGET_CBACT04CBATCHJOBCONFIG-001)")
    public ResponseEntity<GeneratedServiceTargetCbact04cbatchjobconfigResponse> handleGeneratedServiceTargetCbact04cbatchjobconfig001(@Valid @RequestBody(required = false) GeneratedServiceTargetCbact04cbatchjobconfigRequest request) {
        log.info("REST POST /api/v1/core/generated_service_target_cbact04cbatchjobconfig/generatedservicetargetcbact04cbatchjobconfig001 invoked");
        GeneratedServiceTargetCbact04cbatchjobconfigResponse result = service.executeGeneratedServiceTargetCbact04cbatchjobconfig001(request);
        return ResponseEntity.ok(result);
    }
}
