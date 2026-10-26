package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.GeneratedServiceAccountServiceRequest;
import com.client.core.dto.GeneratedServiceAccountServiceResponse;
import com.client.core.service.GeneratedServiceAccountServiceService;
import com.client.core.service.GeneratedServiceAccountServiceService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: GeneratedServiceAccountServiceController
 *
 * RESTful HTTP boundary for modernized COBOL module: generated_service:ACCOUNT-SERVICE
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/generated_service_account_service")
@Tag(name = "GeneratedServiceAccountServiceController", description = "REST API Controller for generated_service:ACCOUNT-SERVICE")
public class GeneratedServiceAccountServiceController {

    private final GeneratedServiceAccountServiceService service;

    public GeneratedServiceAccountServiceController(GeneratedServiceAccountServiceService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies GeneratedServiceAccountServiceService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/generatedserviceaccountservice001")
    @Operation(summary = "Execute generated_service:ACCOUNT-SERVICE Transaction Processing", description = "Endpoint invoking GeneratedServiceAccountServiceService#executeGeneratedServiceAccountService001 (Rule BR-GENERATED_SERVICE:ACCOUNT-SERVICE-001)")
    public ResponseEntity<GeneratedServiceAccountServiceResponse> handleGeneratedServiceAccountService001(@Valid @RequestBody(required = false) GeneratedServiceAccountServiceRequest request) {
        log.info("REST POST /api/v1/core/generated_service_account_service/generatedserviceaccountservice001 invoked");
        GeneratedServiceAccountServiceResponse result = service.executeGeneratedServiceAccountService001(request);
        return ResponseEntity.ok(result);
    }
}
