package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.Cbact02cRequest;
import com.client.core.dto.Cbact02cResponse;
import com.client.core.service.Cbact02cService;
import com.client.core.service.Cbact02cService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: Cbact02cController
 *
 * RESTful HTTP boundary for modernized COBOL module: CBACT02C
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/cbact02c")
@Tag(name = "Cbact02cController", description = "REST API Controller for CBACT02C")
public class Cbact02cController {

    private final Cbact02cService service;

    public Cbact02cController(Cbact02cService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies Cbact02cService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/lookup-xref")
    @Operation(summary = "Lookup Card XREF", description = "Endpoint invoking Cbact02cService#lookupCardXref")
    public ResponseEntity<Cbact02cResponse> handleLookupCardXref(@Valid @RequestBody(required = false) Cbact02cRequest request) {
        return ResponseEntity.ok(service.lookupCardXref(request));
    }

    @PostMapping("/cbact02c001")
    @Operation(summary = "Execute CBACT02C Transaction Processing", description = "Endpoint invoking Cbact02cService#executeCbact02c001 (Rule BR-CBACT02C-001)")
    public ResponseEntity<Cbact02cResponse> handleCbact02c001(@Valid @RequestBody(required = false) Cbact02cRequest request) {
        log.info("REST POST /api/v1/core/cbact02c/cbact02c001 invoked");
        Cbact02cResponse result = service.executeCbact02c001(request);
        return ResponseEntity.ok(result);
    }
}
