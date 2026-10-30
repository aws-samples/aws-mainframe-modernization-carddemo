package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.RuleBrCopaus0c01Request;
import com.client.core.dto.RuleBrCopaus0c01Response;
import com.client.core.service.RuleBrCopaus0c01Service;
import com.client.core.service.RuleBrCopaus0c01Service.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: RuleBrCopaus0c01Controller
 *
 * RESTful HTTP boundary for modernized COBOL module: rule_BR_COPAUS0C_01
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/rule_br_copaus0c_01")
@Tag(name = "RuleBrCopaus0c01Controller", description = "REST API Controller for rule_BR_COPAUS0C_01")
public class RuleBrCopaus0c01Controller {

    private final RuleBrCopaus0c01Service service;

    public RuleBrCopaus0c01Controller(RuleBrCopaus0c01Service service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies RuleBrCopaus0c01Service status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/rulebrcopaus0c01001")
    @Operation(summary = "Execute rule_BR_COPAUS0C_01 Transaction Processing", description = "Endpoint invoking RuleBrCopaus0c01Service#executeRuleBrCopaus0c01001 (Rule BR-RULE_BR_COPAUS0C_01-001)")
    public ResponseEntity<RuleBrCopaus0c01Response> handleRuleBrCopaus0c01001(@Valid @RequestBody(required = false) RuleBrCopaus0c01Request request) {
        log.info("REST POST /api/v1/core/rule_br_copaus0c_01/rulebrcopaus0c01001 invoked");
        RuleBrCopaus0c01Response result = service.executeRuleBrCopaus0c01001(request);
        return ResponseEntity.ok(result);
    }
}
