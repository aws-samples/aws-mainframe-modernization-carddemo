package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.CocrdslcRequest;
import com.client.core.dto.CocrdslcResponse;
import com.client.core.service.CocrdslcService;
import com.client.core.service.CocrdslcService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: CocrdslcController
 *
 * RESTful HTTP boundary for modernized COBOL module: COCRDSLC
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/cocrdslc")
@Tag(name = "CocrdslcController", description = "REST API Controller for COCRDSLC")
public class CocrdslcController {

    private final CocrdslcService service;

    public CocrdslcController(CocrdslcService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies CocrdslcService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/process-card")
    @Operation(summary = "Process Card", description = "Endpoint invoking CocrdslcService#processCard")
    public ResponseEntity<CocrdslcResponse> handleProcessCard(@Valid @RequestBody(required = false) CocrdslcRequest request) {
        return ResponseEntity.ok(service.processCard(request));
    }

    @PostMapping("/cocrdslc001")
    @Operation(summary = "Execute COCRDSLC Transaction Processing", description = "Endpoint invoking CocrdslcService#executeCocrdslc001 (Rule BR-COCRDSLC-001)")
    public ResponseEntity<CocrdslcResponse> handleCocrdslc001(@Valid @RequestBody(required = false) CocrdslcRequest request) {
        log.info("REST POST /api/v1/core/cocrdslc/cocrdslc001 invoked");
        CocrdslcResponse result = service.executeCocrdslc001(request);
        return ResponseEntity.ok(result);
    }
}
