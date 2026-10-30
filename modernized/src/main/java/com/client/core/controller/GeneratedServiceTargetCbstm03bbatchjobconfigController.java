package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.GeneratedServiceTargetCbstm03bbatchjobconfigRequest;
import com.client.core.dto.GeneratedServiceTargetCbstm03bbatchjobconfigResponse;
import com.client.core.service.GeneratedServiceTargetCbstm03bbatchjobconfigService;
import com.client.core.service.GeneratedServiceTargetCbstm03bbatchjobconfigService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: GeneratedServiceTargetCbstm03bbatchjobconfigController
 *
 * RESTful HTTP boundary for modernized COBOL module: generated_service:TARGET_CBSTM03BBATCHJOBCONFIG
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/generated_service_target_cbstm03bbatchjobconfig")
@Tag(name = "GeneratedServiceTargetCbstm03bbatchjobconfigController", description = "REST API Controller for generated_service:TARGET_CBSTM03BBATCHJOBCONFIG")
public class GeneratedServiceTargetCbstm03bbatchjobconfigController {

    private final GeneratedServiceTargetCbstm03bbatchjobconfigService service;

    public GeneratedServiceTargetCbstm03bbatchjobconfigController(GeneratedServiceTargetCbstm03bbatchjobconfigService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies GeneratedServiceTargetCbstm03bbatchjobconfigService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/generatedservicetargetcbstm03bbatchjobconfig001")
    @Operation(summary = "Execute generated_service:TARGET_CBSTM03BBATCHJOBCONFIG Transaction Processing", description = "Endpoint invoking GeneratedServiceTargetCbstm03bbatchjobconfigService#executeGeneratedServiceTargetCbstm03bbatchjobconfig001 (Rule BR-GENERATED_SERVICE:TARGET_CBSTM03BBATCHJOBCONFIG-001)")
    public ResponseEntity<GeneratedServiceTargetCbstm03bbatchjobconfigResponse> handleGeneratedServiceTargetCbstm03bbatchjobconfig001(@Valid @RequestBody(required = false) GeneratedServiceTargetCbstm03bbatchjobconfigRequest request) {
        log.info("REST POST /api/v1/core/generated_service_target_cbstm03bbatchjobconfig/generatedservicetargetcbstm03bbatchjobconfig001 invoked");
        GeneratedServiceTargetCbstm03bbatchjobconfigResponse result = service.executeGeneratedServiceTargetCbstm03bbatchjobconfig001(request);
        return ResponseEntity.ok(result);
    }
}
