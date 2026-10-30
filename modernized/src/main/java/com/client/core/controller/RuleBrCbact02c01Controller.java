package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.RuleBrCbact02c01Request;
import com.client.core.dto.RuleBrCbact02c01Response;
import com.client.core.service.RuleBrCbact02c01Service;
import com.client.core.service.RuleBrCbact02c01Service.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: RuleBrCbact02c01Controller
 *
 * RESTful HTTP boundary for modernized COBOL module: rule_BR_CBACT02C_01
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/rule_br_cbact02c_01")
@Tag(name = "RuleBrCbact02c01Controller", description = "REST API Controller for rule_BR_CBACT02C_01")
public class RuleBrCbact02c01Controller {

    private final RuleBrCbact02c01Service service;

    public RuleBrCbact02c01Controller(RuleBrCbact02c01Service service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies RuleBrCbact02c01Service status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/rulebrcbact02c01001")
    @Operation(summary = "Execute rule_BR_CBACT02C_01 Transaction Processing", description = "Endpoint invoking RuleBrCbact02c01Service#executeRuleBrCbact02c01001 (Rule BR-RULE_BR_CBACT02C_01-001)")
    public ResponseEntity<RuleBrCbact02c01Response> handleRuleBrCbact02c01001(@Valid @RequestBody(required = false) RuleBrCbact02c01Request request) {
        log.info("REST POST /api/v1/core/rule_br_cbact02c_01/rulebrcbact02c01001 invoked");
        RuleBrCbact02c01Response result = service.executeRuleBrCbact02c01001(request);
        return ResponseEntity.ok(result);
    }
}
