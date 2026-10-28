package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.Cbtrn02cRequest;
import com.client.core.dto.Cbtrn02cResponse;
import com.client.core.service.Cbtrn02cService;
import com.client.core.service.Cbtrn02cService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: Cbtrn02cController
 *
 * RESTful HTTP boundary for modernized COBOL module: CBTRN02C
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/cbtrn02c")
@Tag(name = "Cbtrn02cController", description = "REST API Controller for CBTRN02C")
public class Cbtrn02cController {

    private final Cbtrn02cService service;

    public Cbtrn02cController(Cbtrn02cService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies Cbtrn02cService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/process-transaction")
    @Operation(summary = "Post CardDemo Transaction", description = "Endpoint invoking Cbtrn02cService#processTransaction")
    public ResponseEntity<Cbtrn02cResponse> handleProcessTransaction(@Valid @RequestBody(required = false) Cbtrn02cRequest request) {
        return ResponseEntity.ok(service.processTransaction(request));
    }

    @PostMapping("/cbtrn02c001")
    @Operation(summary = "Execute CBTRN02C Transaction Processing", description = "Endpoint invoking Cbtrn02cService#executeCbtrn02c001 (Rule BR-CBTRN02C-001)")
    public ResponseEntity<Cbtrn02cResponse> handleCbtrn02c001(@Valid @RequestBody(required = false) Cbtrn02cRequest request) {
        log.info("REST POST /api/v1/core/cbtrn02c/cbtrn02c001 invoked");
        Cbtrn02cResponse result = service.executeCbtrn02c001(request);
        return ResponseEntity.ok(result);
    }
}
