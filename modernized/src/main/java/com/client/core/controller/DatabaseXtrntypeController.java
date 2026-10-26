package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.DatabaseXtrntypeRequest;
import com.client.core.dto.DatabaseXtrntypeResponse;
import com.client.core.service.DatabaseXtrntypeService;
import com.client.core.service.DatabaseXtrntypeService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: DatabaseXtrntypeController
 *
 * RESTful HTTP boundary for modernized COBOL module: database:XTRNTYPE
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/database_xtrntype")
@Tag(name = "DatabaseXtrntypeController", description = "REST API Controller for database:XTRNTYPE")
public class DatabaseXtrntypeController {

    private final DatabaseXtrntypeService service;

    public DatabaseXtrntypeController(DatabaseXtrntypeService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies DatabaseXtrntypeService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/databasextrntype001")
    @Operation(summary = "Execute database:XTRNTYPE Transaction Processing", description = "Endpoint invoking DatabaseXtrntypeService#executeDatabaseXtrntype001 (Rule BR-DATABASE:XTRNTYPE-001)")
    public ResponseEntity<DatabaseXtrntypeResponse> handleDatabaseXtrntype001(@Valid @RequestBody(required = false) DatabaseXtrntypeRequest request) {
        log.info("REST POST /api/v1/core/database_xtrntype/databasextrntype001 invoked");
        DatabaseXtrntypeResponse result = service.executeDatabaseXtrntype001(request);
        return ResponseEntity.ok(result);
    }
}
