package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.GeneratedServiceTargetCbtrn01cbatchjobconfigRequest;
import com.client.core.dto.GeneratedServiceTargetCbtrn01cbatchjobconfigResponse;
import com.client.core.service.GeneratedServiceTargetCbtrn01cbatchjobconfigService;
import com.client.core.service.GeneratedServiceTargetCbtrn01cbatchjobconfigService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: GeneratedServiceTargetCbtrn01cbatchjobconfigController
 *
 * RESTful HTTP boundary for modernized COBOL module: generated_service:TARGET_CBTRN01CBATCHJOBCONFIG
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/generated_service_target_cbtrn01cbatchjobconfig")
@Tag(name = "GeneratedServiceTargetCbtrn01cbatchjobconfigController", description = "REST API Controller for generated_service:TARGET_CBTRN01CBATCHJOBCONFIG")
public class GeneratedServiceTargetCbtrn01cbatchjobconfigController {

    private final GeneratedServiceTargetCbtrn01cbatchjobconfigService service;

    public GeneratedServiceTargetCbtrn01cbatchjobconfigController(GeneratedServiceTargetCbtrn01cbatchjobconfigService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies GeneratedServiceTargetCbtrn01cbatchjobconfigService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/generatedservicetargetcbtrn01cbatchjobconfig001")
    @Operation(summary = "Execute generated_service:TARGET_CBTRN01CBATCHJOBCONFIG Transaction Processing", description = "Endpoint invoking GeneratedServiceTargetCbtrn01cbatchjobconfigService#executeGeneratedServiceTargetCbtrn01cbatchjobconfig001 (Rule BR-GENERATED_SERVICE:TARGET_CBTRN01CBATCHJOBCONFIG-001)")
    public ResponseEntity<GeneratedServiceTargetCbtrn01cbatchjobconfigResponse> handleGeneratedServiceTargetCbtrn01cbatchjobconfig001(@Valid @RequestBody(required = false) GeneratedServiceTargetCbtrn01cbatchjobconfigRequest request) {
        log.info("REST POST /api/v1/core/generated_service_target_cbtrn01cbatchjobconfig/generatedservicetargetcbtrn01cbatchjobconfig001 invoked");
        GeneratedServiceTargetCbtrn01cbatchjobconfigResponse result = service.executeGeneratedServiceTargetCbtrn01cbatchjobconfig001(request);
        return ResponseEntity.ok(result);
    }
}
