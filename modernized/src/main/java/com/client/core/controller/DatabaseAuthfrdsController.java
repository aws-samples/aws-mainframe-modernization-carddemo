package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.DatabaseAuthfrdsRequest;
import com.client.core.dto.DatabaseAuthfrdsResponse;
import com.client.core.service.DatabaseAuthfrdsService;
import com.client.core.service.DatabaseAuthfrdsService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: DatabaseAuthfrdsController
 *
 * RESTful HTTP boundary for modernized COBOL module: database:AUTHFRDS
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/database_authfrds")
@Tag(name = "DatabaseAuthfrdsController", description = "REST API Controller for database:AUTHFRDS")
public class DatabaseAuthfrdsController {

    private final DatabaseAuthfrdsService service;

    public DatabaseAuthfrdsController(DatabaseAuthfrdsService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies DatabaseAuthfrdsService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/databaseauthfrds001")
    @Operation(summary = "Execute database:AUTHFRDS Transaction Processing", description = "Endpoint invoking DatabaseAuthfrdsService#executeDatabaseAuthfrds001 (Rule BR-DATABASE:AUTHFRDS-001)")
    public ResponseEntity<DatabaseAuthfrdsResponse> handleDatabaseAuthfrds001(@Valid @RequestBody(required = false) DatabaseAuthfrdsRequest request) {
        log.info("REST POST /api/v1/core/database_authfrds/databaseauthfrds001 invoked");
        DatabaseAuthfrdsResponse result = service.executeDatabaseAuthfrds001(request);
        return ResponseEntity.ok(result);
    }
}
