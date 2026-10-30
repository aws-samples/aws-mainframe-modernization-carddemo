package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.GeneratedServiceAuthAdminServiceRequest;
import com.client.core.dto.GeneratedServiceAuthAdminServiceResponse;
import com.client.core.service.GeneratedServiceAuthAdminServiceService;
import com.client.core.service.GeneratedServiceAuthAdminServiceService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: GeneratedServiceAuthAdminServiceController
 *
 * RESTful HTTP boundary for modernized COBOL module: generated_service:AUTH-ADMIN-SERVICE
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/generated_service_auth_admin_service")
@Tag(name = "GeneratedServiceAuthAdminServiceController", description = "REST API Controller for generated_service:AUTH-ADMIN-SERVICE")
public class GeneratedServiceAuthAdminServiceController {

    private final GeneratedServiceAuthAdminServiceService service;

    public GeneratedServiceAuthAdminServiceController(GeneratedServiceAuthAdminServiceService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies GeneratedServiceAuthAdminServiceService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/generatedserviceauthadminservice001")
    @Operation(summary = "Execute generated_service:AUTH-ADMIN-SERVICE Transaction Processing", description = "Endpoint invoking GeneratedServiceAuthAdminServiceService#executeGeneratedServiceAuthAdminService001 (Rule BR-GENERATED_SERVICE:AUTH-ADMIN-SERVICE-001)")
    public ResponseEntity<GeneratedServiceAuthAdminServiceResponse> handleGeneratedServiceAuthAdminService001(@Valid @RequestBody(required = false) GeneratedServiceAuthAdminServiceRequest request) {
        log.info("REST POST /api/v1/core/generated_service_auth_admin_service/generatedserviceauthadminservice001 invoked");
        GeneratedServiceAuthAdminServiceResponse result = service.executeGeneratedServiceAuthAdminService001(request);
        return ResponseEntity.ok(result);
    }
}
