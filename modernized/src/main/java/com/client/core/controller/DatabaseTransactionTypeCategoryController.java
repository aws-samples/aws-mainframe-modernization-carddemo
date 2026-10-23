package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.DatabaseTransactionTypeCategoryRequest;
import com.client.core.dto.DatabaseTransactionTypeCategoryResponse;
import com.client.core.service.DatabaseTransactionTypeCategoryService;
import com.client.core.service.DatabaseTransactionTypeCategoryService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: DatabaseTransactionTypeCategoryController
 *
 * RESTful HTTP boundary for modernized COBOL module: database:TRANSACTION_TYPE_CATEGORY
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/database_transaction_type_category")
@Tag(name = "DatabaseTransactionTypeCategoryController", description = "REST API Controller for database:TRANSACTION_TYPE_CATEGORY")
public class DatabaseTransactionTypeCategoryController {

    private final DatabaseTransactionTypeCategoryService service;

    public DatabaseTransactionTypeCategoryController(DatabaseTransactionTypeCategoryService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies DatabaseTransactionTypeCategoryService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/databasetransactiontypecategory001")
    @Operation(summary = "Execute database:TRANSACTION_TYPE_CATEGORY Transaction Processing", description = "Endpoint invoking DatabaseTransactionTypeCategoryService#executeDatabaseTransactionTypeCategory001 (Rule BR-DATABASE:TRANSACTION_TYPE_CATEGORY-001)")
    public ResponseEntity<DatabaseTransactionTypeCategoryResponse> handleDatabaseTransactionTypeCategory001(@Valid @RequestBody(required = false) DatabaseTransactionTypeCategoryRequest request) {
        log.info("REST POST /api/v1/core/database_transaction_type_category/databasetransactiontypecategory001 invoked");
        DatabaseTransactionTypeCategoryResponse result = service.executeDatabaseTransactionTypeCategory001(request);
        return ResponseEntity.ok(result);
    }
}
