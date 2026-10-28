package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.DatabaseTransactionTypeRequest;
import com.client.core.dto.DatabaseTransactionTypeResponse;
import com.client.core.service.DatabaseTransactionTypeService;
import com.client.core.service.DatabaseTransactionTypeService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: DatabaseTransactionTypeController
 *
 * RESTful HTTP boundary for modernized COBOL module: database:TRANSACTION_TYPE
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/database_transaction_type")
@Tag(name = "DatabaseTransactionTypeController", description = "REST API Controller for database:TRANSACTION_TYPE")
public class DatabaseTransactionTypeController {

    private final DatabaseTransactionTypeService service;

    public DatabaseTransactionTypeController(DatabaseTransactionTypeService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies DatabaseTransactionTypeService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/databasetransactiontype001")
    @Operation(summary = "Execute database:TRANSACTION_TYPE Transaction Processing", description = "Endpoint invoking DatabaseTransactionTypeService#executeDatabaseTransactionType001 (Rule BR-DATABASE:TRANSACTION_TYPE-001)")
    public ResponseEntity<DatabaseTransactionTypeResponse> handleDatabaseTransactionType001(@Valid @RequestBody(required = false) DatabaseTransactionTypeRequest request) {
        log.info("REST POST /api/v1/core/database_transaction_type/databasetransactiontype001 invoked");
        DatabaseTransactionTypeResponse result = service.executeDatabaseTransactionType001(request);
        return ResponseEntity.ok(result);
    }
}
