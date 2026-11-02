package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.RuleBrCobtupdt01Request;
import com.client.core.dto.RuleBrCobtupdt01Response;
import com.client.core.service.RuleBrCobtupdt01Service;
import com.client.core.service.RuleBrCobtupdt01Service.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: RuleBrCobtupdt01Controller
 *
 * RESTful HTTP boundary for modernized COBOL module: rule_BR_COBTUPDT_01
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/rule_br_cobtupdt_01")
@Tag(name = "RuleBrCobtupdt01Controller", description = "REST API Controller for rule_BR_COBTUPDT_01")
public class RuleBrCobtupdt01Controller {

    private final RuleBrCobtupdt01Service service;

    public RuleBrCobtupdt01Controller(RuleBrCobtupdt01Service service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies RuleBrCobtupdt01Service status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/rulebrcobtupdt01001")
    @Operation(summary = "Execute rule_BR_COBTUPDT_01 Transaction Processing", description = "Endpoint invoking RuleBrCobtupdt01Service#executeRuleBrCobtupdt01001 (Rule BR-RULE_BR_COBTUPDT_01-001)")
    public ResponseEntity<RuleBrCobtupdt01Response> handleRuleBrCobtupdt01001(@Valid @RequestBody(required = false) RuleBrCobtupdt01Request request) {
        log.info("REST POST /api/v1/core/rule_br_cobtupdt_01/rulebrcobtupdt01001 invoked");
        RuleBrCobtupdt01Response result = service.executeRuleBrCobtupdt01001(request);
        return ResponseEntity.ok(result);
    }
}
