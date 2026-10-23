package com.client.cics_online.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.cics_online.dto.Copau00cRequest;
import com.client.cics_online.dto.Copau00cResponse;
import com.client.cics_online.service.Copau00cService;
import com.client.cics_online.service.Copau00cService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: Copau00cController
 *
 * RESTful HTTP boundary for modernized COBOL module: COPAU00C
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/cics_online/copau00c")
@Tag(name = "Copau00cController", description = "REST API Controller for COPAU00C")
public class Copau00cController {

    private final Copau00cService service;

    public Copau00cController(Copau00cService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies Copau00cService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/copau00c001")
    @Operation(summary = "Execute COPAU00C Transaction Processing", description = "Endpoint invoking Copau00cService#executeCopau00c001 (Rule BR-COPAU00C-001)")
    public ResponseEntity<Copau00cResponse> handleCopau00c001(@Valid @RequestBody(required = false) Copau00cRequest request) {
        log.info("REST POST /api/v1/cics_online/copau00c/copau00c001 invoked");
        Copau00cResponse result = service.executeCopau00c001(request);
        return ResponseEntity.ok(result);
    }
}
