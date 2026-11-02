package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.RuleBrCbpaup0c01Request;
import com.client.core.dto.RuleBrCbpaup0c01Response;
import com.client.core.service.RuleBrCbpaup0c01Service;
import com.client.core.service.RuleBrCbpaup0c01Service.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: RuleBrCbpaup0c01Controller
 *
 * RESTful HTTP boundary for modernized COBOL module: rule_BR_CBPAUP0C_01
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/rule_br_cbpaup0c_01")
@Tag(name = "RuleBrCbpaup0c01Controller", description = "REST API Controller for rule_BR_CBPAUP0C_01")
public class RuleBrCbpaup0c01Controller {

    private final RuleBrCbpaup0c01Service service;

    public RuleBrCbpaup0c01Controller(RuleBrCbpaup0c01Service service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies RuleBrCbpaup0c01Service status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/rulebrcbpaup0c01001")
    @Operation(summary = "Execute rule_BR_CBPAUP0C_01 Transaction Processing", description = "Endpoint invoking RuleBrCbpaup0c01Service#executeRuleBrCbpaup0c01001 (Rule BR-RULE_BR_CBPAUP0C_01-001)")
    public ResponseEntity<RuleBrCbpaup0c01Response> handleRuleBrCbpaup0c01001(@Valid @RequestBody(required = false) RuleBrCbpaup0c01Request request) {
        log.info("REST POST /api/v1/core/rule_br_cbpaup0c_01/rulebrcbpaup0c01001 invoked");
        RuleBrCbpaup0c01Response result = service.executeRuleBrCbpaup0c01001(request);
        return ResponseEntity.ok(result);
    }
}
