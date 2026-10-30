package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.RuleBrCopaus2c02Request;
import com.client.core.dto.RuleBrCopaus2c02Response;
import com.client.core.service.RuleBrCopaus2c02Service;
import com.client.core.service.RuleBrCopaus2c02Service.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: RuleBrCopaus2c02Controller
 *
 * RESTful HTTP boundary for modernized COBOL module: rule_BR_COPAUS2C_02
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/rule_br_copaus2c_02")
@Tag(name = "RuleBrCopaus2c02Controller", description = "REST API Controller for rule_BR_COPAUS2C_02")
public class RuleBrCopaus2c02Controller {

    private final RuleBrCopaus2c02Service service;

    public RuleBrCopaus2c02Controller(RuleBrCopaus2c02Service service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies RuleBrCopaus2c02Service status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/rulebrcopaus2c02001")
    @Operation(summary = "Execute rule_BR_COPAUS2C_02 Transaction Processing", description = "Endpoint invoking RuleBrCopaus2c02Service#executeRuleBrCopaus2c02001 (Rule BR-RULE_BR_COPAUS2C_02-001)")
    public ResponseEntity<RuleBrCopaus2c02Response> handleRuleBrCopaus2c02001(@Valid @RequestBody(required = false) RuleBrCopaus2c02Request request) {
        log.info("REST POST /api/v1/core/rule_br_copaus2c_02/rulebrcopaus2c02001 invoked");
        RuleBrCopaus2c02Response result = service.executeRuleBrCopaus2c02001(request);
        return ResponseEntity.ok(result);
    }
}
