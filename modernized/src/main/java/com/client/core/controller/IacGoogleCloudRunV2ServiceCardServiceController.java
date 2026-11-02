package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.IacGoogleCloudRunV2ServiceCardServiceRequest;
import com.client.core.dto.IacGoogleCloudRunV2ServiceCardServiceResponse;
import com.client.core.service.IacGoogleCloudRunV2ServiceCardServiceService;
import com.client.core.service.IacGoogleCloudRunV2ServiceCardServiceService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: IacGoogleCloudRunV2ServiceCardServiceController
 *
 * RESTful HTTP boundary for modernized COBOL module: iac_google_cloud_run_v2_service_card_service
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/iac_google_cloud_run_v2_service_card_service")
@Tag(name = "IacGoogleCloudRunV2ServiceCardServiceController", description = "REST API Controller for iac_google_cloud_run_v2_service_card_service")
public class IacGoogleCloudRunV2ServiceCardServiceController {

    private final IacGoogleCloudRunV2ServiceCardServiceService service;

    public IacGoogleCloudRunV2ServiceCardServiceController(IacGoogleCloudRunV2ServiceCardServiceService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies IacGoogleCloudRunV2ServiceCardServiceService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/iacgooglecloudrunv2servicecardservice001")
    @Operation(summary = "Execute iac_google_cloud_run_v2_service_card_service Transaction Processing", description = "Endpoint invoking IacGoogleCloudRunV2ServiceCardServiceService#executeIacGoogleCloudRunV2ServiceCardService001 (Rule BR-IAC_GOOGLE_CLOUD_RUN_V2_SERVICE_CARD_SERVICE-001)")
    public ResponseEntity<IacGoogleCloudRunV2ServiceCardServiceResponse> handleIacGoogleCloudRunV2ServiceCardService001(@Valid @RequestBody(required = false) IacGoogleCloudRunV2ServiceCardServiceRequest request) {
        log.info("REST POST /api/v1/core/iac_google_cloud_run_v2_service_card_service/iacgooglecloudrunv2servicecardservice001 invoked");
        IacGoogleCloudRunV2ServiceCardServiceResponse result = service.executeIacGoogleCloudRunV2ServiceCardService001(request);
        return ResponseEntity.ok(result);
    }
}
