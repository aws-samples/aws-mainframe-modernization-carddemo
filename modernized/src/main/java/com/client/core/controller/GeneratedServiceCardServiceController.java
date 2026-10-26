package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.GeneratedServiceCardServiceRequest;
import com.client.core.dto.GeneratedServiceCardServiceResponse;
import com.client.core.service.GeneratedServiceCardServiceService;
import com.client.core.service.GeneratedServiceCardServiceService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: GeneratedServiceCardServiceController
 *
 * RESTful HTTP boundary for modernized COBOL module: generated_service:CARD-SERVICE
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/generated_service_card_service")
@Tag(name = "GeneratedServiceCardServiceController", description = "REST API Controller for generated_service:CARD-SERVICE")
public class GeneratedServiceCardServiceController {

    private final GeneratedServiceCardServiceService service;

    public GeneratedServiceCardServiceController(GeneratedServiceCardServiceService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies GeneratedServiceCardServiceService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/generatedservicecardservice001")
    @Operation(summary = "Execute generated_service:CARD-SERVICE Transaction Processing", description = "Endpoint invoking GeneratedServiceCardServiceService#executeGeneratedServiceCardService001 (Rule BR-GENERATED_SERVICE:CARD-SERVICE-001)")
    public ResponseEntity<GeneratedServiceCardServiceResponse> handleGeneratedServiceCardService001(@Valid @RequestBody(required = false) GeneratedServiceCardServiceRequest request) {
        log.info("REST POST /api/v1/core/generated_service_card_service/generatedservicecardservice001 invoked");
        GeneratedServiceCardServiceResponse result = service.executeGeneratedServiceCardService001(request);
        return ResponseEntity.ok(result);
    }
}
