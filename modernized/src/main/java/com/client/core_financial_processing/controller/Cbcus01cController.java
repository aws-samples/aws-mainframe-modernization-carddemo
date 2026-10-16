package com.client.core_financial_processing.controller;


import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.client.core_financial_processing.dto.Cbcus01cRequest;
import com.client.core_financial_processing.dto.Cbcus01cResponse;
import com.client.core_financial_processing.service.Cbcus01cService;
import com.client.core_financial_processing.service.Cbcus01cService.ModernizedOperationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Modernized Spring Boot REST Controller: Cbcus01cController
 *
 * RESTful HTTP boundary for modernized COBOL module: CBCUS01C
 * Generation Strategy: Enterprise Modernization
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/core_financial_processing/cbcus01c")
@Tag(name = "Cbcus01cController", description = "REST API Controller for CBCUS01C")
public class Cbcus01cController {

    private final Cbcus01cService service;

    public Cbcus01cController(Cbcus01cService service) {
        this.service = service;
    }

    @GetMapping("/health")
    @Operation(summary = "Health check and module status", description = "Verifies Cbcus01cService status and legacy metadata")
    public ResponseEntity<Map<String, Object>> getHealth() {
        return ResponseEntity.ok(Map.of(
            "moduleId", service.getModuleId(),
            "sourcePath", service.getSourcePath(),
            "status", "ACTIVE",
            "framework", "Spring Boot 3.3.0"
        ));
    }

    @PostMapping("/process-customer")
    @Operation(summary = "Lookup Customer Demographics", description = "Endpoint invoking Cbcus01cService#processCustomer")
    public ResponseEntity<Cbcus01cResponse> handleProcessCustomer(@Valid @RequestBody(required = false) Cbcus01cRequest request) {
        return ResponseEntity.ok(service.processCustomer(request));
    }

    @PostMapping("/cbcus01c0031")
    @Operation(summary = "Business logic rule BR-CBCUS01C-0031", description = "Endpoint invoking Cbcus01cService#executeCbcus01c0031 (Rule BR-CBCUS01C-0031)")
    public ResponseEntity<Cbcus01cResponse> handleCbcus01c0031(@Valid @RequestBody(required = false) Cbcus01cRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbcus01c/cbcus01c0031 invoked");
        Cbcus01cResponse result = service.executeCbcus01c0031(request);
        return ResponseEntity.ok(result);
    }

    @PostMapping("/cbcus01c0032")
    @Operation(summary = "Business logic rule BR-CBCUS01C-0032", description = "Endpoint invoking Cbcus01cService#executeCbcus01c0032 (Rule BR-CBCUS01C-0032)")
    public ResponseEntity<Cbcus01cResponse> handleCbcus01c0032(@Valid @RequestBody(required = false) Cbcus01cRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbcus01c/cbcus01c0032 invoked");
        Cbcus01cResponse result = service.executeCbcus01c0032(request);
        return ResponseEntity.ok(result);
    }

    @PostMapping("/cbcus01c0033")
    @Operation(summary = "Business logic rule BR-CBCUS01C-0033", description = "Endpoint invoking Cbcus01cService#executeCbcus01c0033 (Rule BR-CBCUS01C-0033)")
    public ResponseEntity<Cbcus01cResponse> handleCbcus01c0033(@Valid @RequestBody(required = false) Cbcus01cRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbcus01c/cbcus01c0033 invoked");
        Cbcus01cResponse result = service.executeCbcus01c0033(request);
        return ResponseEntity.ok(result);
    }

    @PostMapping("/cbcus01c0034")
    @Operation(summary = "Business logic rule BR-CBCUS01C-0034", description = "Endpoint invoking Cbcus01cService#executeCbcus01c0034 (Rule BR-CBCUS01C-0034)")
    public ResponseEntity<Cbcus01cResponse> handleCbcus01c0034(@Valid @RequestBody(required = false) Cbcus01cRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbcus01c/cbcus01c0034 invoked");
        Cbcus01cResponse result = service.executeCbcus01c0034(request);
        return ResponseEntity.ok(result);
    }

    @PostMapping("/cbcus01c0035")
    @Operation(summary = "Business logic rule BR-CBCUS01C-0035", description = "Endpoint invoking Cbcus01cService#executeCbcus01c0035 (Rule BR-CBCUS01C-0035)")
    public ResponseEntity<Cbcus01cResponse> handleCbcus01c0035(@Valid @RequestBody(required = false) Cbcus01cRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbcus01c/cbcus01c0035 invoked");
        Cbcus01cResponse result = service.executeCbcus01c0035(request);
        return ResponseEntity.ok(result);
    }

    @PostMapping("/cbcus01c0036")
    @Operation(summary = "Business logic rule BR-CBCUS01C-0036", description = "Endpoint invoking Cbcus01cService#executeCbcus01c0036 (Rule BR-CBCUS01C-0036)")
    public ResponseEntity<Cbcus01cResponse> handleCbcus01c0036(@Valid @RequestBody(required = false) Cbcus01cRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbcus01c/cbcus01c0036 invoked");
        Cbcus01cResponse result = service.executeCbcus01c0036(request);
        return ResponseEntity.ok(result);
    }

    @PostMapping("/cbcus01c0037")
    @Operation(summary = "Business logic rule BR-CBCUS01C-0037", description = "Endpoint invoking Cbcus01cService#executeCbcus01c0037 (Rule BR-CBCUS01C-0037)")
    public ResponseEntity<Cbcus01cResponse> handleCbcus01c0037(@Valid @RequestBody(required = false) Cbcus01cRequest request) {
        log.info("REST POST /api/v1/core_financial_processing/cbcus01c/cbcus01c0037 invoked");
        Cbcus01cResponse result = service.executeCbcus01c0037(request);
        return ResponseEntity.ok(result);
    }
}
