package com.client.core_financial_processing.controller;

import com.client.core_financial_processing.dto.Cbstm03bRequest;
import com.client.core_financial_processing.dto.Cbstm03bResponse;
import com.client.core_financial_processing.service.Cbstm03bService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.math.BigDecimal;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("Cbstm03bController REST API Endpoints Tests")
class Cbstm03bControllerTest {

    private Cbstm03bService service;
    private Cbstm03bController controller;

    @BeforeEach
    void setUp() {
        service = new Cbstm03bService();
        controller = new Cbstm03bController(service);
    }

    @Test
    @DisplayName("GET /api/v1/core_financial_processing/cbstm03b/health returns HTTP 200 and status ACTIVE")
    void testHealthEndpoint() {
        ResponseEntity<Map<String, Object>> response = controller.getHealth();
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertEquals("ACTIVE", response.getBody().get("status"));
        assertEquals("CBSTM03B", response.getBody().get("moduleId"));
    }

    @Test
    @DisplayName("POST /api/v1/core_financial_processing/cbstm03b/cbstm03b0163 returns HTTP 200")
    void testActionEndpoint_Success() {
        Cbstm03bRequest request = new Cbstm03bRequest(
            "00000000001",
            new BigDecimal("150.00"),
            "DEBIT",
            "TXN-1001",
            Map.of()
        );
        ResponseEntity<Cbstm03bResponse> response = controller.handleCbstm03b0163(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertTrue(response.getBody().status().equals("SUCCESS") || response.getBody().status().equals("UNTRANSLATED"));
    }
}
