package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.GeneratedServiceTargetCbcus01cbatchjobconfigRequest;
import com.client.core.dto.GeneratedServiceTargetCbcus01cbatchjobconfigResponse;
import com.client.core.service.GeneratedServiceTargetCbcus01cbatchjobconfigService;
import com.client.core.service.GeneratedServiceTargetCbcus01cbatchjobconfigService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: GeneratedServiceTargetCbcus01cbatchjobconfigController
 *
 * RESTful HTTP boundary for modernized COBOL module: generated_service:TARGET_CBCUS01CBATCHJOBCONFIG
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/generated_service_target_cbcus01cbatchjobconfig")
@Tag(name = "GeneratedServiceTargetCbcus01cbatchjobconfigController", description = "REST API Controller for generated_service:TARGET_CBCUS01CBATCHJOBCONFIG")
public class GeneratedServiceTargetCbcus01cbatchjobconfigController {

    private final GeneratedServiceTargetCbcus01cbatchjobconfigService service;

    public GeneratedServiceTargetCbcus01cbatchjobconfigController(GeneratedServiceTargetCbcus01cbatchjobconfigService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies GeneratedServiceTargetCbcus01cbatchjobconfigService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/generatedservicetargetcbcus01cbatchjobconfig001")
    @Operation(summary = "Execute generated_service:TARGET_CBCUS01CBATCHJOBCONFIG Transaction Processing", description = "Endpoint invoking GeneratedServiceTargetCbcus01cbatchjobconfigService#executeGeneratedServiceTargetCbcus01cbatchjobconfig001 (Rule BR-GENERATED_SERVICE:TARGET_CBCUS01CBATCHJOBCONFIG-001)")
    public ResponseEntity<GeneratedServiceTargetCbcus01cbatchjobconfigResponse> handleGeneratedServiceTargetCbcus01cbatchjobconfig001(@Valid @RequestBody(required = false) GeneratedServiceTargetCbcus01cbatchjobconfigRequest request) {
        log.info("REST POST /api/v1/core/generated_service_target_cbcus01cbatchjobconfig/generatedservicetargetcbcus01cbatchjobconfig001 invoked");
        GeneratedServiceTargetCbcus01cbatchjobconfigResponse result = service.executeGeneratedServiceTargetCbcus01cbatchjobconfig001(request);
        return ResponseEntity.ok(result);
    }
}
