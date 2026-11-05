package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.RuleBrCbact01c02Request;
import com.client.core.dto.RuleBrCbact01c02Response;
import com.client.core.service.RuleBrCbact01c02Service;
import com.client.core.service.RuleBrCbact01c02Service.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: RuleBrCbact01c02Controller
 *
 * RESTful HTTP boundary for modernized COBOL module: rule_BR_CBACT01C_02
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/rule_br_cbact01c_02")
@Tag(name = "RuleBrCbact01c02Controller", description = "REST API Controller for rule_BR_CBACT01C_02")
public class RuleBrCbact01c02Controller {

    private final RuleBrCbact01c02Service service;

    public RuleBrCbact01c02Controller(RuleBrCbact01c02Service service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies RuleBrCbact01c02Service status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/rulebrcbact01c02001")
    @Operation(summary = "Execute rule_BR_CBACT01C_02 Transaction Processing", description = "Endpoint invoking RuleBrCbact01c02Service#executeRuleBrCbact01c02001 (Rule BR-RULE_BR_CBACT01C_02-001)")
    public ResponseEntity<RuleBrCbact01c02Response> handleRuleBrCbact01c02001(@Valid @RequestBody(required = false) RuleBrCbact01c02Request request) {
        log.info("REST POST /api/v1/core/rule_br_cbact01c_02/rulebrcbact01c02001 invoked");
        RuleBrCbact01c02Response result = service.executeRuleBrCbact01c02001(request);
        return ResponseEntity.ok(result);
    }
}
