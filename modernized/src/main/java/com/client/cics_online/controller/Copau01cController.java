package com.client.cics_online.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.cics_online.dto.Copau01cRequest;
import com.client.cics_online.dto.Copau01cResponse;
import com.client.cics_online.service.Copau01cService;
import com.client.cics_online.service.Copau01cService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: Copau01cController
 *
 * RESTful HTTP boundary for modernized COBOL module: COPAU01C
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/cics_online/copau01c")
@Tag(name = "Copau01cController", description = "REST API Controller for COPAU01C")
public class Copau01cController {

    private final Copau01cService service;

    public Copau01cController(Copau01cService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies Copau01cService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/copau01c001")
    @Operation(summary = "Execute COPAU01C Transaction Processing", description = "Endpoint invoking Copau01cService#executeCopau01c001 (Rule BR-COPAU01C-001)")
    public ResponseEntity<Copau01cResponse> handleCopau01c001(@Valid @RequestBody(required = false) Copau01cRequest request) {
        log.info("REST POST /api/v1/cics_online/copau01c/copau01c001 invoked");
        Copau01cResponse result = service.executeCopau01c001(request);
        return ResponseEntity.ok(result);
    }
}
