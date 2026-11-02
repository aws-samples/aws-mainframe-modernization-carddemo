package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.IacGoogleCloudRunV2ServiceAccountServiceRequest;
import com.client.core.dto.IacGoogleCloudRunV2ServiceAccountServiceResponse;
import com.client.core.service.IacGoogleCloudRunV2ServiceAccountServiceService;
import com.client.core.service.IacGoogleCloudRunV2ServiceAccountServiceService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: IacGoogleCloudRunV2ServiceAccountServiceController
 *
 * RESTful HTTP boundary for modernized COBOL module: iac_google_cloud_run_v2_service_account_service
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/iac_google_cloud_run_v2_service_account_service")
@Tag(name = "IacGoogleCloudRunV2ServiceAccountServiceController", description = "REST API Controller for iac_google_cloud_run_v2_service_account_service")
public class IacGoogleCloudRunV2ServiceAccountServiceController {

    private final IacGoogleCloudRunV2ServiceAccountServiceService service;

    public IacGoogleCloudRunV2ServiceAccountServiceController(IacGoogleCloudRunV2ServiceAccountServiceService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies IacGoogleCloudRunV2ServiceAccountServiceService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/iacgooglecloudrunv2serviceaccountservice001")
    @Operation(summary = "Execute iac_google_cloud_run_v2_service_account_service Transaction Processing", description = "Endpoint invoking IacGoogleCloudRunV2ServiceAccountServiceService#executeIacGoogleCloudRunV2ServiceAccountService001 (Rule BR-IAC_GOOGLE_CLOUD_RUN_V2_SERVICE_ACCOUNT_SERVICE-001)")
    public ResponseEntity<IacGoogleCloudRunV2ServiceAccountServiceResponse> handleIacGoogleCloudRunV2ServiceAccountService001(@Valid @RequestBody(required = false) IacGoogleCloudRunV2ServiceAccountServiceRequest request) {
        log.info("REST POST /api/v1/core/iac_google_cloud_run_v2_service_account_service/iacgooglecloudrunv2serviceaccountservice001 invoked");
        IacGoogleCloudRunV2ServiceAccountServiceResponse result = service.executeIacGoogleCloudRunV2ServiceAccountService001(request);
        return ResponseEntity.ok(result);
    }
}
