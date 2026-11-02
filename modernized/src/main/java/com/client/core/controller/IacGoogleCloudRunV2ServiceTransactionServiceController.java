package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.IacGoogleCloudRunV2ServiceTransactionServiceRequest;
import com.client.core.dto.IacGoogleCloudRunV2ServiceTransactionServiceResponse;
import com.client.core.service.IacGoogleCloudRunV2ServiceTransactionServiceService;
import com.client.core.service.IacGoogleCloudRunV2ServiceTransactionServiceService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: IacGoogleCloudRunV2ServiceTransactionServiceController
 *
 * RESTful HTTP boundary for modernized COBOL module: iac_google_cloud_run_v2_service_transaction_service
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/iac_google_cloud_run_v2_service_transaction_service")
@Tag(name = "IacGoogleCloudRunV2ServiceTransactionServiceController", description = "REST API Controller for iac_google_cloud_run_v2_service_transaction_service")
public class IacGoogleCloudRunV2ServiceTransactionServiceController {

    private final IacGoogleCloudRunV2ServiceTransactionServiceService service;

    public IacGoogleCloudRunV2ServiceTransactionServiceController(IacGoogleCloudRunV2ServiceTransactionServiceService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies IacGoogleCloudRunV2ServiceTransactionServiceService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/iacgooglecloudrunv2servicetransactionservice001")
    @Operation(summary = "Execute iac_google_cloud_run_v2_service_transaction_service Transaction Processing", description = "Endpoint invoking IacGoogleCloudRunV2ServiceTransactionServiceService#executeIacGoogleCloudRunV2ServiceTransactionService001 (Rule BR-IAC_GOOGLE_CLOUD_RUN_V2_SERVICE_TRANSACTION_SERVICE-001)")
    public ResponseEntity<IacGoogleCloudRunV2ServiceTransactionServiceResponse> handleIacGoogleCloudRunV2ServiceTransactionService001(@Valid @RequestBody(required = false) IacGoogleCloudRunV2ServiceTransactionServiceRequest request) {
        log.info("REST POST /api/v1/core/iac_google_cloud_run_v2_service_transaction_service/iacgooglecloudrunv2servicetransactionservice001 invoked");
        IacGoogleCloudRunV2ServiceTransactionServiceResponse result = service.executeIacGoogleCloudRunV2ServiceTransactionService001(request);
        return ResponseEntity.ok(result);
    }
}
