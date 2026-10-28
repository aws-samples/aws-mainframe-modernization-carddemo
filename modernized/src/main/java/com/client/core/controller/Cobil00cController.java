package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.Cobil00cRequest;
import com.client.core.dto.Cobil00cResponse;
import com.client.core.service.Cobil00cService;
import com.client.core.service.Cobil00cService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: Cobil00cController
 *
 * RESTful HTTP boundary for modernized COBOL module: COBIL00C
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/cobil00c")
@Tag(name = "Cobil00cController", description = "REST API Controller for COBIL00C")
public class Cobil00cController {

    private final Cobil00cService service;

    public Cobil00cController(Cobil00cService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies Cobil00cService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/process-payment")
    @Operation(summary = "Process Bill Payment", description = "Endpoint invoking Cobil00cService#processBillPayment")
    public ResponseEntity<Cobil00cResponse> handleProcessPayment(@Valid @RequestBody(required = false) Cobil00cRequest request) {
        return ResponseEntity.ok(service.processBillPayment(request));
    }

    @PostMapping("/cobil00c001")
    @Operation(summary = "Execute COBIL00C Transaction Processing", description = "Endpoint invoking Cobil00cService#executeCobil00c001 (Rule BR-COBIL00C-001)")
    public ResponseEntity<Cobil00cResponse> handleCobil00c001(@Valid @RequestBody(required = false) Cobil00cRequest request) {
        log.info("REST POST /api/v1/core/cobil00c/cobil00c001 invoked");
        Cobil00cResponse result = service.executeCobil00c001(request);
        return ResponseEntity.ok(result);
    }
}
