package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.GeneratedServiceTargetCbact01cbatchjobconfigRequest;
import com.client.core.dto.GeneratedServiceTargetCbact01cbatchjobconfigResponse;
import com.client.core.service.GeneratedServiceTargetCbact01cbatchjobconfigService;
import com.client.core.service.GeneratedServiceTargetCbact01cbatchjobconfigService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: GeneratedServiceTargetCbact01cbatchjobconfigController
 *
 * RESTful HTTP boundary for modernized COBOL module: generated_service:TARGET_CBACT01CBATCHJOBCONFIG
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/generated_service_target_cbact01cbatchjobconfig")
@Tag(name = "GeneratedServiceTargetCbact01cbatchjobconfigController", description = "REST API Controller for generated_service:TARGET_CBACT01CBATCHJOBCONFIG")
public class GeneratedServiceTargetCbact01cbatchjobconfigController {

    private final GeneratedServiceTargetCbact01cbatchjobconfigService service;

    public GeneratedServiceTargetCbact01cbatchjobconfigController(GeneratedServiceTargetCbact01cbatchjobconfigService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies GeneratedServiceTargetCbact01cbatchjobconfigService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/generatedservicetargetcbact01cbatchjobconfig001")
    @Operation(summary = "Execute generated_service:TARGET_CBACT01CBATCHJOBCONFIG Transaction Processing", description = "Endpoint invoking GeneratedServiceTargetCbact01cbatchjobconfigService#executeGeneratedServiceTargetCbact01cbatchjobconfig001 (Rule BR-GENERATED_SERVICE:TARGET_CBACT01CBATCHJOBCONFIG-001)")
    public ResponseEntity<GeneratedServiceTargetCbact01cbatchjobconfigResponse> handleGeneratedServiceTargetCbact01cbatchjobconfig001(@Valid @RequestBody(required = false) GeneratedServiceTargetCbact01cbatchjobconfigRequest request) {
        log.info("REST POST /api/v1/core/generated_service_target_cbact01cbatchjobconfig/generatedservicetargetcbact01cbatchjobconfig001 invoked");
        GeneratedServiceTargetCbact01cbatchjobconfigResponse result = service.executeGeneratedServiceTargetCbact01cbatchjobconfig001(request);
        return ResponseEntity.ok(result);
    }
}
