package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.IacGoogleCloudRunV2JobBatchOrchestrationServiceRequest;
import com.client.core.dto.IacGoogleCloudRunV2JobBatchOrchestrationServiceResponse;
import com.client.core.service.IacGoogleCloudRunV2JobBatchOrchestrationServiceService;
import com.client.core.service.IacGoogleCloudRunV2JobBatchOrchestrationServiceService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: IacGoogleCloudRunV2JobBatchOrchestrationServiceController
 *
 * RESTful HTTP boundary for modernized COBOL module: iac_google_cloud_run_v2_job_batch_orchestration_service
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/iac_google_cloud_run_v2_job_batch_orchestration_service")
@Tag(name = "IacGoogleCloudRunV2JobBatchOrchestrationServiceController", description = "REST API Controller for iac_google_cloud_run_v2_job_batch_orchestration_service")
public class IacGoogleCloudRunV2JobBatchOrchestrationServiceController {

    private final IacGoogleCloudRunV2JobBatchOrchestrationServiceService service;

    public IacGoogleCloudRunV2JobBatchOrchestrationServiceController(IacGoogleCloudRunV2JobBatchOrchestrationServiceService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies IacGoogleCloudRunV2JobBatchOrchestrationServiceService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/iacgooglecloudrunv2jobbatchorchestrationservice001")
    @Operation(summary = "Execute iac_google_cloud_run_v2_job_batch_orchestration_service Transaction Processing", description = "Endpoint invoking IacGoogleCloudRunV2JobBatchOrchestrationServiceService#executeIacGoogleCloudRunV2JobBatchOrchestrationService001 (Rule BR-IAC_GOOGLE_CLOUD_RUN_V2_JOB_BATCH_ORCHESTRATION_SERVICE-001)")
    public ResponseEntity<IacGoogleCloudRunV2JobBatchOrchestrationServiceResponse> handleIacGoogleCloudRunV2JobBatchOrchestrationService001(@Valid @RequestBody(required = false) IacGoogleCloudRunV2JobBatchOrchestrationServiceRequest request) {
        log.info("REST POST /api/v1/core/iac_google_cloud_run_v2_job_batch_orchestration_service/iacgooglecloudrunv2jobbatchorchestrationservice001 invoked");
        IacGoogleCloudRunV2JobBatchOrchestrationServiceResponse result = service.executeIacGoogleCloudRunV2JobBatchOrchestrationService001(request);
        return ResponseEntity.ok(result);
    }
}
