package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.DatabasePautindxRequest;
import com.client.core.dto.DatabasePautindxResponse;
import com.client.core.service.DatabasePautindxService;
import com.client.core.service.DatabasePautindxService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: DatabasePautindxController
 *
 * RESTful HTTP boundary for modernized COBOL module: database:PAUTINDX
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/database_pautindx")
@Tag(name = "DatabasePautindxController", description = "REST API Controller for database:PAUTINDX")
public class DatabasePautindxController {

    private final DatabasePautindxService service;

    public DatabasePautindxController(DatabasePautindxService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies DatabasePautindxService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/databasepautindx001")
    @Operation(summary = "Execute database:PAUTINDX Transaction Processing", description = "Endpoint invoking DatabasePautindxService#executeDatabasePautindx001 (Rule BR-DATABASE:PAUTINDX-001)")
    public ResponseEntity<DatabasePautindxResponse> handleDatabasePautindx001(@Valid @RequestBody(required = false) DatabasePautindxRequest request) {
        log.info("REST POST /api/v1/core/database_pautindx/databasepautindx001 invoked");
        DatabasePautindxResponse result = service.executeDatabasePautindx001(request);
        return ResponseEntity.ok(result);
    }
}
