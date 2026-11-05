package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.RuleBrCoacct0101Request;
import com.client.core.dto.RuleBrCoacct0101Response;
import com.client.core.service.RuleBrCoacct0101Service;
import com.client.core.service.RuleBrCoacct0101Service.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: RuleBrCoacct0101Controller
 *
 * RESTful HTTP boundary for modernized COBOL module: rule_BR_COACCT01_01
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/rule_br_coacct01_01")
@Tag(name = "RuleBrCoacct0101Controller", description = "REST API Controller for rule_BR_COACCT01_01")
public class RuleBrCoacct0101Controller {

    private final RuleBrCoacct0101Service service;

    public RuleBrCoacct0101Controller(RuleBrCoacct0101Service service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies RuleBrCoacct0101Service status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/rulebrcoacct0101001")
    @Operation(summary = "Execute rule_BR_COACCT01_01 Transaction Processing", description = "Endpoint invoking RuleBrCoacct0101Service#executeRuleBrCoacct0101001 (Rule BR-RULE_BR_COACCT01_01-001)")
    public ResponseEntity<RuleBrCoacct0101Response> handleRuleBrCoacct0101001(@Valid @RequestBody(required = false) RuleBrCoacct0101Request request) {
        log.info("REST POST /api/v1/core/rule_br_coacct01_01/rulebrcoacct0101001 invoked");
        RuleBrCoacct0101Response result = service.executeRuleBrCoacct0101001(request);
        return ResponseEntity.ok(result);
    }
}
