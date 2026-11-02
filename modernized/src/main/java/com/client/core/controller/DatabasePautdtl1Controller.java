package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.DatabasePautdtl1Request;
import com.client.core.dto.DatabasePautdtl1Response;
import com.client.core.service.DatabasePautdtl1Service;
import com.client.core.service.DatabasePautdtl1Service.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: DatabasePautdtl1Controller
 *
 * RESTful HTTP boundary for modernized COBOL module: database:PAUTDTL1
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/database_pautdtl1")
@Tag(name = "DatabasePautdtl1Controller", description = "REST API Controller for database:PAUTDTL1")
public class DatabasePautdtl1Controller {

    private final DatabasePautdtl1Service service;

    public DatabasePautdtl1Controller(DatabasePautdtl1Service service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies DatabasePautdtl1Service status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/databasepautdtl1001")
    @Operation(summary = "Execute database:PAUTDTL1 Transaction Processing", description = "Endpoint invoking DatabasePautdtl1Service#executeDatabasePautdtl1001 (Rule BR-DATABASE:PAUTDTL1-001)")
    public ResponseEntity<DatabasePautdtl1Response> handleDatabasePautdtl1001(@Valid @RequestBody(required = false) DatabasePautdtl1Request request) {
        log.info("REST POST /api/v1/core/database_pautdtl1/databasepautdtl1001 invoked");
        DatabasePautdtl1Response result = service.executeDatabasePautdtl1001(request);
        return ResponseEntity.ok(result);
    }
}
