package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.DatabaseXtrntycatRequest;
import com.client.core.dto.DatabaseXtrntycatResponse;
import com.client.core.service.DatabaseXtrntycatService;
import com.client.core.service.DatabaseXtrntycatService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: DatabaseXtrntycatController
 *
 * RESTful HTTP boundary for modernized COBOL module: database:XTRNTYCAT
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/database_xtrntycat")
@Tag(name = "DatabaseXtrntycatController", description = "REST API Controller for database:XTRNTYCAT")
public class DatabaseXtrntycatController {

    private final DatabaseXtrntycatService service;

    public DatabaseXtrntycatController(DatabaseXtrntycatService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies DatabaseXtrntycatService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/databasextrntycat001")
    @Operation(summary = "Execute database:XTRNTYCAT Transaction Processing", description = "Endpoint invoking DatabaseXtrntycatService#executeDatabaseXtrntycat001 (Rule BR-DATABASE:XTRNTYCAT-001)")
    public ResponseEntity<DatabaseXtrntycatResponse> handleDatabaseXtrntycat001(@Valid @RequestBody(required = false) DatabaseXtrntycatRequest request) {
        log.info("REST POST /api/v1/core/database_xtrntycat/databasextrntycat001 invoked");
        DatabaseXtrntycatResponse result = service.executeDatabaseXtrntycat001(request);
        return ResponseEntity.ok(result);
    }
}
