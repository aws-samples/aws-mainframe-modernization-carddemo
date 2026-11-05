package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.IacGoogleSqlDatabaseInstanceCarddemoPostgresPrimaryRequest;
import com.client.core.dto.IacGoogleSqlDatabaseInstanceCarddemoPostgresPrimaryResponse;
import com.client.core.service.IacGoogleSqlDatabaseInstanceCarddemoPostgresPrimaryService;
import com.client.core.service.IacGoogleSqlDatabaseInstanceCarddemoPostgresPrimaryService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: IacGoogleSqlDatabaseInstanceCarddemoPostgresPrimaryController
 *
 * RESTful HTTP boundary for modernized COBOL module: iac_google_sql_database_instance_carddemo_postgres_primary
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/iac_google_sql_database_instance_carddemo_postgres_primary")
@Tag(name = "IacGoogleSqlDatabaseInstanceCarddemoPostgresPrimaryController", description = "REST API Controller for iac_google_sql_database_instance_carddemo_postgres_primary")
public class IacGoogleSqlDatabaseInstanceCarddemoPostgresPrimaryController {

    private final IacGoogleSqlDatabaseInstanceCarddemoPostgresPrimaryService service;

    public IacGoogleSqlDatabaseInstanceCarddemoPostgresPrimaryController(IacGoogleSqlDatabaseInstanceCarddemoPostgresPrimaryService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies IacGoogleSqlDatabaseInstanceCarddemoPostgresPrimaryService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/iacgooglesqldatabaseinstancecarddemopostgresprimary001")
    @Operation(summary = "Execute iac_google_sql_database_instance_carddemo_postgres_primary Transaction Processing", description = "Endpoint invoking IacGoogleSqlDatabaseInstanceCarddemoPostgresPrimaryService#executeIacGoogleSqlDatabaseInstanceCarddemoPostgresPrimary001 (Rule BR-IAC_GOOGLE_SQL_DATABASE_INSTANCE_CARDDEMO_POSTGRES_PRIMARY-001)")
    public ResponseEntity<IacGoogleSqlDatabaseInstanceCarddemoPostgresPrimaryResponse> handleIacGoogleSqlDatabaseInstanceCarddemoPostgresPrimary001(@Valid @RequestBody(required = false) IacGoogleSqlDatabaseInstanceCarddemoPostgresPrimaryRequest request) {
        log.info("REST POST /api/v1/core/iac_google_sql_database_instance_carddemo_postgres_primary/iacgooglesqldatabaseinstancecarddemopostgresprimary001 invoked");
        IacGoogleSqlDatabaseInstanceCarddemoPostgresPrimaryResponse result = service.executeIacGoogleSqlDatabaseInstanceCarddemoPostgresPrimary001(request);
        return ResponseEntity.ok(result);
    }
}
