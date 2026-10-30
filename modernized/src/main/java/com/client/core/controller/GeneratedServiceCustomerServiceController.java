package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.GeneratedServiceCustomerServiceRequest;
import com.client.core.dto.GeneratedServiceCustomerServiceResponse;
import com.client.core.service.GeneratedServiceCustomerServiceService;
import com.client.core.service.GeneratedServiceCustomerServiceService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: GeneratedServiceCustomerServiceController
 *
 * RESTful HTTP boundary for modernized COBOL module: generated_service:CUSTOMER-SERVICE
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/generated_service_customer_service")
@Tag(name = "GeneratedServiceCustomerServiceController", description = "REST API Controller for generated_service:CUSTOMER-SERVICE")
public class GeneratedServiceCustomerServiceController {

    private final GeneratedServiceCustomerServiceService service;

    public GeneratedServiceCustomerServiceController(GeneratedServiceCustomerServiceService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies GeneratedServiceCustomerServiceService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/generatedservicecustomerservice001")
    @Operation(summary = "Execute generated_service:CUSTOMER-SERVICE Transaction Processing", description = "Endpoint invoking GeneratedServiceCustomerServiceService#executeGeneratedServiceCustomerService001 (Rule BR-GENERATED_SERVICE:CUSTOMER-SERVICE-001)")
    public ResponseEntity<GeneratedServiceCustomerServiceResponse> handleGeneratedServiceCustomerService001(@Valid @RequestBody(required = false) GeneratedServiceCustomerServiceRequest request) {
        log.info("REST POST /api/v1/core/generated_service_customer_service/generatedservicecustomerservice001 invoked");
        GeneratedServiceCustomerServiceResponse result = service.executeGeneratedServiceCustomerService001(request);
        return ResponseEntity.ok(result);
    }
}
