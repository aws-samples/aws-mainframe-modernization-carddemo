package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.CocrdupcRequest;
import com.client.core.dto.CocrdupcResponse;
import com.client.core.service.CocrdupcService;
import com.client.core.service.CocrdupcService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: CocrdupcController
 *
 * RESTful HTTP boundary for modernized COBOL module: COCRDUPC
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/cocrdupc")
@Tag(name = "CocrdupcController", description = "REST API Controller for COCRDUPC")
public class CocrdupcController {

    private final CocrdupcService service;

    public CocrdupcController(CocrdupcService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies CocrdupcService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/process-card")
    @Operation(summary = "Process Card", description = "Endpoint invoking CocrdupcService#processCard")
    public ResponseEntity<CocrdupcResponse> handleProcessCard(@Valid @RequestBody(required = false) CocrdupcRequest request) {
        return ResponseEntity.ok(service.processCard(request));
    }

    @PostMapping("/cocrdupc001")
    @Operation(summary = "Execute COCRDUPC Transaction Processing", description = "Endpoint invoking CocrdupcService#executeCocrdupc001 (Rule BR-COCRDUPC-001)")
    public ResponseEntity<CocrdupcResponse> handleCocrdupc001(@Valid @RequestBody(required = false) CocrdupcRequest request) {
        log.info("REST POST /api/v1/core/cocrdupc/cocrdupc001 invoked");
        CocrdupcResponse result = service.executeCocrdupc001(request);
        return ResponseEntity.ok(result);
    }
}
