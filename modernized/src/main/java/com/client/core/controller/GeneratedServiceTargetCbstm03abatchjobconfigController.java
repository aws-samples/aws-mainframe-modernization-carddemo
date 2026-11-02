package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.GeneratedServiceTargetCbstm03abatchjobconfigRequest;
import com.client.core.dto.GeneratedServiceTargetCbstm03abatchjobconfigResponse;
import com.client.core.service.GeneratedServiceTargetCbstm03abatchjobconfigService;
import com.client.core.service.GeneratedServiceTargetCbstm03abatchjobconfigService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: GeneratedServiceTargetCbstm03abatchjobconfigController
 *
 * RESTful HTTP boundary for modernized COBOL module: generated_service:TARGET_CBSTM03ABATCHJOBCONFIG
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/generated_service_target_cbstm03abatchjobconfig")
@Tag(name = "GeneratedServiceTargetCbstm03abatchjobconfigController", description = "REST API Controller for generated_service:TARGET_CBSTM03ABATCHJOBCONFIG")
public class GeneratedServiceTargetCbstm03abatchjobconfigController {

    private final GeneratedServiceTargetCbstm03abatchjobconfigService service;

    public GeneratedServiceTargetCbstm03abatchjobconfigController(GeneratedServiceTargetCbstm03abatchjobconfigService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies GeneratedServiceTargetCbstm03abatchjobconfigService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/generatedservicetargetcbstm03abatchjobconfig001")
    @Operation(summary = "Execute generated_service:TARGET_CBSTM03ABATCHJOBCONFIG Transaction Processing", description = "Endpoint invoking GeneratedServiceTargetCbstm03abatchjobconfigService#executeGeneratedServiceTargetCbstm03abatchjobconfig001 (Rule BR-GENERATED_SERVICE:TARGET_CBSTM03ABATCHJOBCONFIG-001)")
    public ResponseEntity<GeneratedServiceTargetCbstm03abatchjobconfigResponse> handleGeneratedServiceTargetCbstm03abatchjobconfig001(@Valid @RequestBody(required = false) GeneratedServiceTargetCbstm03abatchjobconfigRequest request) {
        log.info("REST POST /api/v1/core/generated_service_target_cbstm03abatchjobconfig/generatedservicetargetcbstm03abatchjobconfig001 invoked");
        GeneratedServiceTargetCbstm03abatchjobconfigResponse result = service.executeGeneratedServiceTargetCbstm03abatchjobconfig001(request);
        return ResponseEntity.ok(result);
    }
}
