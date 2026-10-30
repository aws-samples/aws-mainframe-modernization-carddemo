package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.DatabaseXauthfrdRequest;
import com.client.core.dto.DatabaseXauthfrdResponse;
import com.client.core.service.DatabaseXauthfrdService;
import com.client.core.service.DatabaseXauthfrdService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: DatabaseXauthfrdController
 *
 * RESTful HTTP boundary for modernized COBOL module: database:XAUTHFRD
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/database_xauthfrd")
@Tag(name = "DatabaseXauthfrdController", description = "REST API Controller for database:XAUTHFRD")
public class DatabaseXauthfrdController {

    private final DatabaseXauthfrdService service;

    public DatabaseXauthfrdController(DatabaseXauthfrdService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies DatabaseXauthfrdService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/databasexauthfrd001")
    @Operation(summary = "Execute database:XAUTHFRD Transaction Processing", description = "Endpoint invoking DatabaseXauthfrdService#executeDatabaseXauthfrd001 (Rule BR-DATABASE:XAUTHFRD-001)")
    public ResponseEntity<DatabaseXauthfrdResponse> handleDatabaseXauthfrd001(@Valid @RequestBody(required = false) DatabaseXauthfrdRequest request) {
        log.info("REST POST /api/v1/core/database_xauthfrd/databasexauthfrd001 invoked");
        DatabaseXauthfrdResponse result = service.executeDatabaseXauthfrd001(request);
        return ResponseEntity.ok(result);
    }
}
