package com.client.core.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core.dto.RuleBrCobtupdt02Request;
import com.client.core.dto.RuleBrCobtupdt02Response;
import com.client.core.service.RuleBrCobtupdt02Service;
import com.client.core.service.RuleBrCobtupdt02Service.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: RuleBrCobtupdt02Controller
 *
 * RESTful HTTP boundary for modernized COBOL module: rule_BR_COBTUPDT_02
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core/rule_br_cobtupdt_02")
@Tag(name = "RuleBrCobtupdt02Controller", description = "REST API Controller for rule_BR_COBTUPDT_02")
public class RuleBrCobtupdt02Controller {

    private final RuleBrCobtupdt02Service service;

    public RuleBrCobtupdt02Controller(RuleBrCobtupdt02Service service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies RuleBrCobtupdt02Service status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/rulebrcobtupdt02001")
    @Operation(summary = "Execute rule_BR_COBTUPDT_02 Transaction Processing", description = "Endpoint invoking RuleBrCobtupdt02Service#executeRuleBrCobtupdt02001 (Rule BR-RULE_BR_COBTUPDT_02-001)")
    public ResponseEntity<RuleBrCobtupdt02Response> handleRuleBrCobtupdt02001(@Valid @RequestBody(required = false) RuleBrCobtupdt02Request request) {
        log.info("REST POST /api/v1/core/rule_br_cobtupdt_02/rulebrcobtupdt02001 invoked");
        RuleBrCobtupdt02Response result = service.executeRuleBrCobtupdt02001(request);
        return ResponseEntity.ok(result);
    }
}
