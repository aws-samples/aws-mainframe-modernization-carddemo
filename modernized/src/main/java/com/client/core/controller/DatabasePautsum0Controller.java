package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.DatabasePautsum0Request;
import com.client.core.dto.DatabasePautsum0Response;
import com.client.core.service.DatabasePautsum0Service;
import com.client.core.service.DatabasePautsum0Service.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: DatabasePautsum0Controller
 *
 * RESTful HTTP boundary for modernized COBOL module: database:PAUTSUM0
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/database_pautsum0")
@Tag(name = "DatabasePautsum0Controller", description = "REST API Controller for database:PAUTSUM0")
public class DatabasePautsum0Controller {

    private final DatabasePautsum0Service service;

    public DatabasePautsum0Controller(DatabasePautsum0Service service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies DatabasePautsum0Service status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/databasepautsum0001")
    @Operation(summary = "Execute database:PAUTSUM0 Transaction Processing", description = "Endpoint invoking DatabasePautsum0Service#executeDatabasePautsum0001 (Rule BR-DATABASE:PAUTSUM0-001)")
    public ResponseEntity<DatabasePautsum0Response> handleDatabasePautsum0001(@Valid @RequestBody(required = false) DatabasePautsum0Request request) {
        log.info("REST POST /api/v1/core/database_pautsum0/databasepautsum0001 invoked");
        DatabasePautsum0Response result = service.executeDatabasePautsum0001(request);
        return ResponseEntity.ok(result);
    }
}
