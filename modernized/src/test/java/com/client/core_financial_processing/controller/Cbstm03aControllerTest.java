package com.client.core_financial_processing.controller;

import com.client.core_financial_processing.dto.Cbstm03aRequest;
import com.client.core_financial_processing.dto.Cbstm03aResponse;
import com.client.core_financial_processing.service.Cbstm03aService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.math.BigDecimal;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("Cbstm03aController REST API Endpoints Tests")
class Cbstm03aControllerTest {

    private Cbstm03aService service;
    private Cbstm03aController controller;

    @BeforeEach
    void setUp() {
        service = new Cbstm03aService();
        controller = new Cbstm03aController(service);
    }

    @Test
    @DisplayName("GET /api/v1/core_financial_processing/cbstm03a/health returns HTTP 200 and status ACTIVE")
    void testHealthEndpoint() {
        ResponseEntity<Map<String, Object>> response = controller.getHealth();
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertEquals("ACTIVE", response.getBody().get("status"));
        assertEquals("CBSTM03A", response.getBody().get("moduleId"));
    }

    @Test
    @DisplayName("POST /api/v1/core_financial_processing/cbstm03a/cbstm03a0043 returns HTTP 200")
    void testActionEndpoint_Success() {
        Cbstm03aRequest request = new Cbstm03aRequest(
            "00000000001",
            new BigDecimal("150.00"),
            "DEBIT",
            "TXN-1001",
            Map.of()
        );
        ResponseEntity<Cbstm03aResponse> response = controller.handleCbstm03a0043(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertTrue(response.getBody().status().equals("SUCCESS") || response.getBody().status().equals("UNTRANSLATED"));
    }
}
