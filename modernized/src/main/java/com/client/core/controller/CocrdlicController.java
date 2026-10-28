package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.CocrdlicRequest;
import com.client.core.dto.CocrdlicResponse;
import com.client.core.service.CocrdlicService;
import com.client.core.service.CocrdlicService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: CocrdlicController
 *
 * RESTful HTTP boundary for modernized COBOL module: COCRDLIC
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/cocrdlic")
@Tag(name = "CocrdlicController", description = "REST API Controller for COCRDLIC")
public class CocrdlicController {

    private final CocrdlicService service;

    public CocrdlicController(CocrdlicService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies CocrdlicService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/process-card")
    @Operation(summary = "Process Card", description = "Endpoint invoking CocrdlicService#processCard")
    public ResponseEntity<CocrdlicResponse> handleProcessCard(@Valid @RequestBody(required = false) CocrdlicRequest request) {
        return ResponseEntity.ok(service.processCard(request));
    }

    @PostMapping("/cocrdlic001")
    @Operation(summary = "Execute COCRDLIC Transaction Processing", description = "Endpoint invoking CocrdlicService#executeCocrdlic001 (Rule BR-COCRDLIC-001)")
    public ResponseEntity<CocrdlicResponse> handleCocrdlic001(@Valid @RequestBody(required = false) CocrdlicRequest request) {
        log.info("REST POST /api/v1/core/cocrdlic/cocrdlic001 invoked");
        CocrdlicResponse result = service.executeCocrdlic001(request);
        return ResponseEntity.ok(result);
    }
}
