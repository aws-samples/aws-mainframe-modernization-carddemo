package com.client.core_financial_processing.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core_financial_processing.dto.CsutldtcRequest;
import com.client.core_financial_processing.dto.CsutldtcResponse;
import com.client.core_financial_processing.service.CsutldtcService;
import com.client.core_financial_processing.service.CsutldtcService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: CsutldtcController
 *
 * RESTful HTTP boundary for modernized COBOL module: CSUTLDTC
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core_financial_processing/csutldtc")
@Tag(name = "CsutldtcController", description = "REST API Controller for CSUTLDTC")
public class CsutldtcController {

    private final CsutldtcService service;

    public CsutldtcController(CsutldtcService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies CsutldtcService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/validate-date")
    @Operation(summary = "Validate Date using CEEDAYS Rules", description = "Endpoint invoking CsutldtcService#validateDate")
    public ResponseEntity<CsutldtcResponse> handleValidateDate(@Valid @RequestBody(required = false) CsutldtcRequest request) {
        return ResponseEntity.ok(service.validateDate(request));
    }

    @PostMapping("/csutldtc0283")
    @Operation(summary = "Business logic rule BR-CSUTLDTC-0283", description = "Endpoint invoking CsutldtcService#executeCsutldtc0283 (Rule BR-CSUTLDTC-0283)")
    public ResponseEntity<CsutldtcResponse> handleCsutldtc0283(@Valid @RequestBody(required = false) CsutldtcRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/csutldtc/csutldtc0283 invoked");
        CsutldtcResponse result = service.executeCsutldtc0283(request);
        return ResponseEntity.ok(result);
    }

    @PostMapping("/csutldtc0284")
    @Operation(summary = "Business logic rule BR-CSUTLDTC-0284", description = "Endpoint invoking CsutldtcService#executeCsutldtc0284 (Rule BR-CSUTLDTC-0284)")
    public ResponseEntity<CsutldtcResponse> handleCsutldtc0284(@Valid @RequestBody(required = false) CsutldtcRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/csutldtc/csutldtc0284 invoked");
        CsutldtcResponse result = service.executeCsutldtc0284(request);
        return ResponseEntity.ok(result);
    }

    @PostMapping("/csutldtc0285")
    @Operation(summary = "Business logic rule BR-CSUTLDTC-0285", description = "Endpoint invoking CsutldtcService#executeCsutldtc0285 (Rule BR-CSUTLDTC-0285)")
    public ResponseEntity<CsutldtcResponse> handleCsutldtc0285(@Valid @RequestBody(required = false) CsutldtcRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/csutldtc/csutldtc0285 invoked");
        CsutldtcResponse result = service.executeCsutldtc0285(request);
        return ResponseEntity.ok(result);
    }
}
