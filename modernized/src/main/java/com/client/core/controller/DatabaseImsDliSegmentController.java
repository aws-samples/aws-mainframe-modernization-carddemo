package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.DatabaseImsDliSegmentRequest;
import com.client.core.dto.DatabaseImsDliSegmentResponse;
import com.client.core.service.DatabaseImsDliSegmentService;
import com.client.core.service.DatabaseImsDliSegmentService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: DatabaseImsDliSegmentController
 *
 * RESTful HTTP boundary for modernized COBOL module: database:IMS_DLI_SEGMENT
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/database_ims_dli_segment")
@Tag(name = "DatabaseImsDliSegmentController", description = "REST API Controller for database:IMS_DLI_SEGMENT")
public class DatabaseImsDliSegmentController {

    private final DatabaseImsDliSegmentService service;

    public DatabaseImsDliSegmentController(DatabaseImsDliSegmentService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies DatabaseImsDliSegmentService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/databaseimsdlisegment001")
    @Operation(summary = "Execute database:IMS_DLI_SEGMENT Transaction Processing", description = "Endpoint invoking DatabaseImsDliSegmentService#executeDatabaseImsDliSegment001 (Rule BR-DATABASE:IMS_DLI_SEGMENT-001)")
    public ResponseEntity<DatabaseImsDliSegmentResponse> handleDatabaseImsDliSegment001(@Valid @RequestBody(required = false) DatabaseImsDliSegmentRequest request) {
        log.info("REST POST /api/v1/core/database_ims_dli_segment/databaseimsdlisegment001 invoked");
        DatabaseImsDliSegmentResponse result = service.executeDatabaseImsDliSegment001(request);
        return ResponseEntity.ok(result);
    }
}
