package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.DatabaseCarddemoAuthfrdsRequest;
import com.client.core.dto.DatabaseCarddemoAuthfrdsResponse;
import com.client.core.service.DatabaseCarddemoAuthfrdsService;
import com.client.core.service.DatabaseCarddemoAuthfrdsService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: DatabaseCarddemoAuthfrdsController
 *
 * RESTful HTTP boundary for modernized COBOL module: database:CARDDEMO.AUTHFRDS
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/database_carddemo_authfrds")
@Tag(name = "DatabaseCarddemoAuthfrdsController", description = "REST API Controller for database:CARDDEMO.AUTHFRDS")
public class DatabaseCarddemoAuthfrdsController {

    private final DatabaseCarddemoAuthfrdsService service;

    public DatabaseCarddemoAuthfrdsController(DatabaseCarddemoAuthfrdsService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies DatabaseCarddemoAuthfrdsService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/databasecarddemoauthfrds001")
    @Operation(summary = "Execute database:CARDDEMO.AUTHFRDS Transaction Processing", description = "Endpoint invoking DatabaseCarddemoAuthfrdsService#executeDatabaseCarddemoAuthfrds001 (Rule BR-DATABASE:CARDDEMO.AUTHFRDS-001)")
    public ResponseEntity<DatabaseCarddemoAuthfrdsResponse> handleDatabaseCarddemoAuthfrds001(@Valid @RequestBody(required = false) DatabaseCarddemoAuthfrdsRequest request) {
        log.info("REST POST /api/v1/core/database_carddemo_authfrds/databasecarddemoauthfrds001 invoked");
        DatabaseCarddemoAuthfrdsResponse result = service.executeDatabaseCarddemoAuthfrds001(request);
        return ResponseEntity.ok(result);
    }
}
