package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.RuleBrCbpaup0c02Request;
import com.client.core.dto.RuleBrCbpaup0c02Response;
import com.client.core.service.RuleBrCbpaup0c02Service;
import com.client.core.service.RuleBrCbpaup0c02Service.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: RuleBrCbpaup0c02Controller
 *
 * RESTful HTTP boundary for modernized COBOL module: rule_BR_CBPAUP0C_02
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/rule_br_cbpaup0c_02")
@Tag(name = "RuleBrCbpaup0c02Controller", description = "REST API Controller for rule_BR_CBPAUP0C_02")
public class RuleBrCbpaup0c02Controller {

    private final RuleBrCbpaup0c02Service service;

    public RuleBrCbpaup0c02Controller(RuleBrCbpaup0c02Service service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies RuleBrCbpaup0c02Service status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/rulebrcbpaup0c02001")
    @Operation(summary = "Execute rule_BR_CBPAUP0C_02 Transaction Processing", description = "Endpoint invoking RuleBrCbpaup0c02Service#executeRuleBrCbpaup0c02001 (Rule BR-RULE_BR_CBPAUP0C_02-001)")
    public ResponseEntity<RuleBrCbpaup0c02Response> handleRuleBrCbpaup0c02001(@Valid @RequestBody(required = false) RuleBrCbpaup0c02Request request) {
        log.info("REST POST /api/v1/core/rule_br_cbpaup0c_02/rulebrcbpaup0c02001 invoked");
        RuleBrCbpaup0c02Response result = service.executeRuleBrCbpaup0c02001(request);
        return ResponseEntity.ok(result);
    }
}
