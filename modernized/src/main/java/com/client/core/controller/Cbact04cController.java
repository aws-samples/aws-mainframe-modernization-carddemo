package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.Cbact04cRequest;
import com.client.core.dto.Cbact04cResponse;
import com.client.core.service.Cbact04cService;
import com.client.core.service.Cbact04cService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: Cbact04cController
 *
 * RESTful HTTP boundary for modernized COBOL module: CBACT04C
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/cbact04c")
@Tag(name = "Cbact04cController", description = "REST API Controller for CBACT04C")
public class Cbact04cController {

    private final Cbact04cService service;

    public Cbact04cController(Cbact04cService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies Cbact04cService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/calculate-interest")
    @Operation(summary = "Calculate Monthly Interest", description = "Endpoint invoking Cbact04cService#calculateMonthlyInterest")
    public ResponseEntity<Cbact04cResponse> handleCalculateInterest(@Valid @RequestBody(required = false) Cbact04cRequest request) {
        return ResponseEntity.ok(service.calculateMonthlyInterest(request));
    }

    @PostMapping("/cbact04c001")
    @Operation(summary = "Execute CBACT04C Transaction Processing", description = "Endpoint invoking Cbact04cService#executeCbact04c001 (Rule BR-CBACT04C-001)")
    public ResponseEntity<Cbact04cResponse> handleCbact04c001(@Valid @RequestBody(required = false) Cbact04cRequest request) {
        log.info("REST POST /api/v1/core/cbact04c/cbact04c001 invoked");
        Cbact04cResponse result = service.executeCbact04c001(request);
        return ResponseEntity.ok(result);
    }
}
