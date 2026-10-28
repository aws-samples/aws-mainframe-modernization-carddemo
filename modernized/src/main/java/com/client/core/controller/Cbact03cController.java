package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.Cbact03cRequest;
import com.client.core.dto.Cbact03cResponse;
import com.client.core.service.Cbact03cService;
import com.client.core.service.Cbact03cService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: Cbact03cController
 *
 * RESTful HTTP boundary for modernized COBOL module: CBACT03C
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/cbact03c")
@Tag(name = "Cbact03cController", description = "REST API Controller for CBACT03C")
public class Cbact03cController {

    private final Cbact03cService service;

    public Cbact03cController(Cbact03cService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies Cbact03cService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/create-xref")
    @Operation(summary = "Create Card XREF", description = "Endpoint invoking Cbact03cService#createCardXref")
    public ResponseEntity<Cbact03cResponse> handleCreateCardXref(@Valid @RequestBody(required = false) Cbact03cRequest request) {
        return ResponseEntity.ok(service.createCardXref(request));
    }

    @PostMapping("/cbact03c001")
    @Operation(summary = "Execute CBACT03C Transaction Processing", description = "Endpoint invoking Cbact03cService#executeCbact03c001 (Rule BR-CBACT03C-001)")
    public ResponseEntity<Cbact03cResponse> handleCbact03c001(@Valid @RequestBody(required = false) Cbact03cRequest request) {
        log.info("REST POST /api/v1/core/cbact03c/cbact03c001 invoked");
        Cbact03cResponse result = service.executeCbact03c001(request);
        return ResponseEntity.ok(result);
    }
}
