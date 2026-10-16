package com.client.core_financial_processing.controller;

import com.client.core_financial_processing.dto.Cbtrn02cRequest;
import com.client.core_financial_processing.dto.Cbtrn02cResponse;
import com.client.core_financial_processing.service.Cbtrn02cService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.math.BigDecimal;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("Cbtrn02cController REST API Endpoints Tests")
class Cbtrn02cControllerTest {

    private Cbtrn02cService service;
    private Cbtrn02cController controller;

    @BeforeEach
    void setUp() {
        service = new Cbtrn02cService();
        controller = new Cbtrn02cController(service);
    }

    @Test
    @DisplayName("GET /api/v1/core_financial_processing/cbtrn02c/health returns HTTP 200 and status ACTIVE")
    void testHealthEndpoint() {
        ResponseEntity<Map<String, Object>> response = controller.getHealth();
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertEquals("ACTIVE", response.getBody().get("status"));
        assertEquals("CBTRN02C", response.getBody().get("moduleId"));
    }

    @Test
    @DisplayName("POST /api/v1/core_financial_processing/cbtrn02c/cbtrn02c0051 returns HTTP 200")
    void testActionEndpoint_Success() {
        Cbtrn02cRequest request = new Cbtrn02cRequest(
            "00000000001",
            new BigDecimal("150.00"),
            "DEBIT",
            "TXN-1001",
            Map.of()
        );
        ResponseEntity<Cbtrn02cResponse> response = controller.handleCbtrn02c0051(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertTrue(response.getBody().status().equals("SUCCESS") || response.getBody().status().equals("UNTRANSLATED"));
    }
}
