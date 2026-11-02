package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.GeneratedServiceBatchOrchestrationServiceRequest;
import com.client.core.dto.GeneratedServiceBatchOrchestrationServiceResponse;
import com.client.core.service.GeneratedServiceBatchOrchestrationServiceService;
import com.client.core.service.GeneratedServiceBatchOrchestrationServiceService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: GeneratedServiceBatchOrchestrationServiceController
 *
 * RESTful HTTP boundary for modernized COBOL module: generated_service:BATCH-ORCHESTRATION-SERVICE
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/generated_service_batch_orchestration_service")
@Tag(name = "GeneratedServiceBatchOrchestrationServiceController", description = "REST API Controller for generated_service:BATCH-ORCHESTRATION-SERVICE")
public class GeneratedServiceBatchOrchestrationServiceController {

    private final GeneratedServiceBatchOrchestrationServiceService service;

    public GeneratedServiceBatchOrchestrationServiceController(GeneratedServiceBatchOrchestrationServiceService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies GeneratedServiceBatchOrchestrationServiceService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/generatedservicebatchorchestrationservice001")
    @Operation(summary = "Execute generated_service:BATCH-ORCHESTRATION-SERVICE Transaction Processing", description = "Endpoint invoking GeneratedServiceBatchOrchestrationServiceService#executeGeneratedServiceBatchOrchestrationService001 (Rule BR-GENERATED_SERVICE:BATCH-ORCHESTRATION-SERVICE-001)")
    public ResponseEntity<GeneratedServiceBatchOrchestrationServiceResponse> handleGeneratedServiceBatchOrchestrationService001(@Valid @RequestBody(required = false) GeneratedServiceBatchOrchestrationServiceRequest request) {
        log.info("REST POST /api/v1/core/generated_service_batch_orchestration_service/generatedservicebatchorchestrationservice001 invoked");
        GeneratedServiceBatchOrchestrationServiceResponse result = service.executeGeneratedServiceBatchOrchestrationService001(request);
        return ResponseEntity.ok(result);
    }
}
