package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.Cbact01cRequest;
import com.client.core.dto.Cbact01cResponse;
import com.client.core.service.Cbact01cService;
import com.client.core.service.Cbact01cService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: Cbact01cController
 *
 * RESTful HTTP boundary for modernized COBOL module: CBACT01C
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/cbact01c")
@Tag(name = "Cbact01cController", description = "REST API Controller for CBACT01C")
public class Cbact01cController {

    private final Cbact01cService service;

    public Cbact01cController(Cbact01cService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies Cbact01cService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/lookup-account")
    @Operation(summary = "Lookup Account Record", description = "Endpoint invoking Cbact01cService#lookupAccount")
    public ResponseEntity<Cbact01cResponse> handleLookupAccount(@Valid @RequestBody(required = false) Cbact01cRequest request) {
        return ResponseEntity.ok(service.lookupAccount(request));
    }

    @PostMapping("/cbact01c001")
    @Operation(summary = "Execute CBACT01C Transaction Processing", description = "Endpoint invoking Cbact01cService#executeCbact01c001 (Rule BR-CBACT01C-001)")
    public ResponseEntity<Cbact01cResponse> handleCbact01c001(@Valid @RequestBody(required = false) Cbact01cRequest request) {
        log.info("REST POST /api/v1/core/cbact01c/cbact01c001 invoked");
        Cbact01cResponse result = service.executeCbact01c001(request);
        return ResponseEntity.ok(result);
    }
}
