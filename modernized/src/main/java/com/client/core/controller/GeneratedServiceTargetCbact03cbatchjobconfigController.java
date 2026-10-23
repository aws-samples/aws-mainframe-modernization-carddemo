package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.GeneratedServiceTargetCbact03cbatchjobconfigRequest;
import com.client.core.dto.GeneratedServiceTargetCbact03cbatchjobconfigResponse;
import com.client.core.service.GeneratedServiceTargetCbact03cbatchjobconfigService;
import com.client.core.service.GeneratedServiceTargetCbact03cbatchjobconfigService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: GeneratedServiceTargetCbact03cbatchjobconfigController
 *
 * RESTful HTTP boundary for modernized COBOL module: generated_service:TARGET_CBACT03CBATCHJOBCONFIG
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/generated_service_target_cbact03cbatchjobconfig")
@Tag(name = "GeneratedServiceTargetCbact03cbatchjobconfigController", description = "REST API Controller for generated_service:TARGET_CBACT03CBATCHJOBCONFIG")
public class GeneratedServiceTargetCbact03cbatchjobconfigController {

    private final GeneratedServiceTargetCbact03cbatchjobconfigService service;

    public GeneratedServiceTargetCbact03cbatchjobconfigController(GeneratedServiceTargetCbact03cbatchjobconfigService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies GeneratedServiceTargetCbact03cbatchjobconfigService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/generatedservicetargetcbact03cbatchjobconfig001")
    @Operation(summary = "Execute generated_service:TARGET_CBACT03CBATCHJOBCONFIG Transaction Processing", description = "Endpoint invoking GeneratedServiceTargetCbact03cbatchjobconfigService#executeGeneratedServiceTargetCbact03cbatchjobconfig001 (Rule BR-GENERATED_SERVICE:TARGET_CBACT03CBATCHJOBCONFIG-001)")
    public ResponseEntity<GeneratedServiceTargetCbact03cbatchjobconfigResponse> handleGeneratedServiceTargetCbact03cbatchjobconfig001(@Valid @RequestBody(required = false) GeneratedServiceTargetCbact03cbatchjobconfigRequest request) {
        log.info("REST POST /api/v1/core/generated_service_target_cbact03cbatchjobconfig/generatedservicetargetcbact03cbatchjobconfig001 invoked");
        GeneratedServiceTargetCbact03cbatchjobconfigResponse result = service.executeGeneratedServiceTargetCbact03cbatchjobconfig001(request);
        return ResponseEntity.ok(result);
    }
}
