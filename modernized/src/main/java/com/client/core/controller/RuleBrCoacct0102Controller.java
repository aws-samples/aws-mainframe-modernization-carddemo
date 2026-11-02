package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.RuleBrCoacct0102Request;
import com.client.core.dto.RuleBrCoacct0102Response;
import com.client.core.service.RuleBrCoacct0102Service;
import com.client.core.service.RuleBrCoacct0102Service.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: RuleBrCoacct0102Controller
 *
 * RESTful HTTP boundary for modernized COBOL module: rule_BR_COACCT01_02
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/rule_br_coacct01_02")
@Tag(name = "RuleBrCoacct0102Controller", description = "REST API Controller for rule_BR_COACCT01_02")
public class RuleBrCoacct0102Controller {

    private final RuleBrCoacct0102Service service;

    public RuleBrCoacct0102Controller(RuleBrCoacct0102Service service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies RuleBrCoacct0102Service status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/rulebrcoacct0102001")
    @Operation(summary = "Execute rule_BR_COACCT01_02 Transaction Processing", description = "Endpoint invoking RuleBrCoacct0102Service#executeRuleBrCoacct0102001 (Rule BR-RULE_BR_COACCT01_02-001)")
    public ResponseEntity<RuleBrCoacct0102Response> handleRuleBrCoacct0102001(@Valid @RequestBody(required = false) RuleBrCoacct0102Request request) {
        log.info("REST POST /api/v1/core/rule_br_coacct01_02/rulebrcoacct0102001 invoked");
        RuleBrCoacct0102Response result = service.executeRuleBrCoacct0102001(request);
        return ResponseEntity.ok(result);
    }
}
