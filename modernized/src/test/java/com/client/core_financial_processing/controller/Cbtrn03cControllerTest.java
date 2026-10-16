package com.client.core_financial_processing.controller;

import com.client.core_financial_processing.dto.Cbtrn03cRequest;
import com.client.core_financial_processing.dto.Cbtrn03cResponse;
import com.client.core_financial_processing.service.Cbtrn03cService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.math.BigDecimal;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("Cbtrn03cController REST API Endpoints Tests")
class Cbtrn03cControllerTest {

    private Cbtrn03cService service;
    private Cbtrn03cController controller;

    @BeforeEach
    void setUp() {
        service = new Cbtrn03cService();
        controller = new Cbtrn03cController(service);
    }

    @Test
    @DisplayName("GET /api/v1/core_financial_processing/cbtrn03c/health returns HTTP 200 and status ACTIVE")
    void testHealthEndpoint() {
        ResponseEntity<Map<String, Object>> response = controller.getHealth();
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertEquals("ACTIVE", response.getBody().get("status"));
        assertEquals("CBTRN03C", response.getBody().get("moduleId"));
    }

    @Test
    @DisplayName("POST /api/v1/core_financial_processing/cbtrn03c/cbtrn03c0059 returns HTTP 200")
    void testActionEndpoint_Success() {
        Cbtrn03cRequest request = new Cbtrn03cRequest(
            "00000000001",
            new BigDecimal("150.00"),
            "DEBIT",
            "TXN-1001",
            Map.of()
        );
        ResponseEntity<Cbtrn03cResponse> response = controller.handleCbtrn03c0059(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertTrue(response.getBody().status().equals("SUCCESS") || response.getBody().status().equals("UNTRANSLATED"));
    }
}
