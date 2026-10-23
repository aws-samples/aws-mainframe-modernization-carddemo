package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.GeneratedServiceTargetCbact02cbatchjobconfigRequest;
import com.client.core.dto.GeneratedServiceTargetCbact02cbatchjobconfigResponse;
import com.client.core.service.GeneratedServiceTargetCbact02cbatchjobconfigService;
import com.client.core.service.GeneratedServiceTargetCbact02cbatchjobconfigService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: GeneratedServiceTargetCbact02cbatchjobconfigController
 *
 * RESTful HTTP boundary for modernized COBOL module: generated_service:TARGET_CBACT02CBATCHJOBCONFIG
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/generated_service_target_cbact02cbatchjobconfig")
@Tag(name = "GeneratedServiceTargetCbact02cbatchjobconfigController", description = "REST API Controller for generated_service:TARGET_CBACT02CBATCHJOBCONFIG")
public class GeneratedServiceTargetCbact02cbatchjobconfigController {

    private final GeneratedServiceTargetCbact02cbatchjobconfigService service;

    public GeneratedServiceTargetCbact02cbatchjobconfigController(GeneratedServiceTargetCbact02cbatchjobconfigService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies GeneratedServiceTargetCbact02cbatchjobconfigService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/generatedservicetargetcbact02cbatchjobconfig001")
    @Operation(summary = "Execute generated_service:TARGET_CBACT02CBATCHJOBCONFIG Transaction Processing", description = "Endpoint invoking GeneratedServiceTargetCbact02cbatchjobconfigService#executeGeneratedServiceTargetCbact02cbatchjobconfig001 (Rule BR-GENERATED_SERVICE:TARGET_CBACT02CBATCHJOBCONFIG-001)")
    public ResponseEntity<GeneratedServiceTargetCbact02cbatchjobconfigResponse> handleGeneratedServiceTargetCbact02cbatchjobconfig001(@Valid @RequestBody(required = false) GeneratedServiceTargetCbact02cbatchjobconfigRequest request) {
        log.info("REST POST /api/v1/core/generated_service_target_cbact02cbatchjobconfig/generatedservicetargetcbact02cbatchjobconfig001 invoked");
        GeneratedServiceTargetCbact02cbatchjobconfigResponse result = service.executeGeneratedServiceTargetCbact02cbatchjobconfig001(request);
        return ResponseEntity.ok(result);
    }
}
