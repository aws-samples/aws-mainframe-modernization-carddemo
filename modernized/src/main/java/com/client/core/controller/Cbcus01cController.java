package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.Cbcus01cRequest;
import com.client.core.dto.Cbcus01cResponse;
import com.client.core.service.Cbcus01cService;
import com.client.core.service.Cbcus01cService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: Cbcus01cController
 *
 * RESTful HTTP boundary for modernized COBOL module: CBCUS01C
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/cbcus01c")
@Tag(name = "Cbcus01cController", description = "REST API Controller for CBCUS01C")
public class Cbcus01cController {

    private final Cbcus01cService service;

    public Cbcus01cController(Cbcus01cService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies Cbcus01cService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/process-customer")
    @Operation(summary = "Lookup Customer Demographics", description = "Endpoint invoking Cbcus01cService#processCustomer")
    public ResponseEntity<Cbcus01cResponse> handleProcessCustomer(@Valid @RequestBody(required = false) Cbcus01cRequest request) {
        return ResponseEntity.ok(service.processCustomer(request));
    }

    @PostMapping("/cbcus01c001")
    @Operation(summary = "Execute CBCUS01C Transaction Processing", description = "Endpoint invoking Cbcus01cService#executeCbcus01c001 (Rule BR-CBCUS01C-001)")
    public ResponseEntity<Cbcus01cResponse> handleCbcus01c001(@Valid @RequestBody(required = false) Cbcus01cRequest request) {
        log.info("REST POST /api/v1/core/cbcus01c/cbcus01c001 invoked");
        Cbcus01cResponse result = service.executeCbcus01c001(request);
        return ResponseEntity.ok(result);
    }
}
