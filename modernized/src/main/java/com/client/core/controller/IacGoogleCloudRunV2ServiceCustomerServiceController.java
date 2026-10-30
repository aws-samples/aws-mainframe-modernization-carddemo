package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.IacGoogleCloudRunV2ServiceCustomerServiceRequest;
import com.client.core.dto.IacGoogleCloudRunV2ServiceCustomerServiceResponse;
import com.client.core.service.IacGoogleCloudRunV2ServiceCustomerServiceService;
import com.client.core.service.IacGoogleCloudRunV2ServiceCustomerServiceService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: IacGoogleCloudRunV2ServiceCustomerServiceController
 *
 * RESTful HTTP boundary for modernized COBOL module: iac_google_cloud_run_v2_service_customer_service
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/iac_google_cloud_run_v2_service_customer_service")
@Tag(name = "IacGoogleCloudRunV2ServiceCustomerServiceController", description = "REST API Controller for iac_google_cloud_run_v2_service_customer_service")
public class IacGoogleCloudRunV2ServiceCustomerServiceController {

    private final IacGoogleCloudRunV2ServiceCustomerServiceService service;

    public IacGoogleCloudRunV2ServiceCustomerServiceController(IacGoogleCloudRunV2ServiceCustomerServiceService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies IacGoogleCloudRunV2ServiceCustomerServiceService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/iacgooglecloudrunv2servicecustomerservice001")
    @Operation(summary = "Execute iac_google_cloud_run_v2_service_customer_service Transaction Processing", description = "Endpoint invoking IacGoogleCloudRunV2ServiceCustomerServiceService#executeIacGoogleCloudRunV2ServiceCustomerService001 (Rule BR-IAC_GOOGLE_CLOUD_RUN_V2_SERVICE_CUSTOMER_SERVICE-001)")
    public ResponseEntity<IacGoogleCloudRunV2ServiceCustomerServiceResponse> handleIacGoogleCloudRunV2ServiceCustomerService001(@Valid @RequestBody(required = false) IacGoogleCloudRunV2ServiceCustomerServiceRequest request) {
        log.info("REST POST /api/v1/core/iac_google_cloud_run_v2_service_customer_service/iacgooglecloudrunv2servicecustomerservice001 invoked");
        IacGoogleCloudRunV2ServiceCustomerServiceResponse result = service.executeIacGoogleCloudRunV2ServiceCustomerService001(request);
        return ResponseEntity.ok(result);
    }
}
