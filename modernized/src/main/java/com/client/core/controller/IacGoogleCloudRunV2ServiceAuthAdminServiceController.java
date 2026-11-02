package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.IacGoogleCloudRunV2ServiceAuthAdminServiceRequest;
import com.client.core.dto.IacGoogleCloudRunV2ServiceAuthAdminServiceResponse;
import com.client.core.service.IacGoogleCloudRunV2ServiceAuthAdminServiceService;
import com.client.core.service.IacGoogleCloudRunV2ServiceAuthAdminServiceService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: IacGoogleCloudRunV2ServiceAuthAdminServiceController
 *
 * RESTful HTTP boundary for modernized COBOL module: iac_google_cloud_run_v2_service_auth_admin_service
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/iac_google_cloud_run_v2_service_auth_admin_service")
@Tag(name = "IacGoogleCloudRunV2ServiceAuthAdminServiceController", description = "REST API Controller for iac_google_cloud_run_v2_service_auth_admin_service")
public class IacGoogleCloudRunV2ServiceAuthAdminServiceController {

    private final IacGoogleCloudRunV2ServiceAuthAdminServiceService service;

    public IacGoogleCloudRunV2ServiceAuthAdminServiceController(IacGoogleCloudRunV2ServiceAuthAdminServiceService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies IacGoogleCloudRunV2ServiceAuthAdminServiceService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/iacgooglecloudrunv2serviceauthadminservice001")
    @Operation(summary = "Execute iac_google_cloud_run_v2_service_auth_admin_service Transaction Processing", description = "Endpoint invoking IacGoogleCloudRunV2ServiceAuthAdminServiceService#executeIacGoogleCloudRunV2ServiceAuthAdminService001 (Rule BR-IAC_GOOGLE_CLOUD_RUN_V2_SERVICE_AUTH_ADMIN_SERVICE-001)")
    public ResponseEntity<IacGoogleCloudRunV2ServiceAuthAdminServiceResponse> handleIacGoogleCloudRunV2ServiceAuthAdminService001(@Valid @RequestBody(required = false) IacGoogleCloudRunV2ServiceAuthAdminServiceRequest request) {
        log.info("REST POST /api/v1/core/iac_google_cloud_run_v2_service_auth_admin_service/iacgooglecloudrunv2serviceauthadminservice001 invoked");
        IacGoogleCloudRunV2ServiceAuthAdminServiceResponse result = service.executeIacGoogleCloudRunV2ServiceAuthAdminService001(request);
        return ResponseEntity.ok(result);
    }
}
