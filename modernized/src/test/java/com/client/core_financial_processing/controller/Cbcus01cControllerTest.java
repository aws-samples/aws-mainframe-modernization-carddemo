package com.client.core_financial_processing.controller;

import com.client.core_financial_processing.dto.Cbcus01cRequest;
import com.client.core_financial_processing.dto.Cbcus01cResponse;
import com.client.core_financial_processing.service.Cbcus01cService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.math.BigDecimal;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("Cbcus01cController REST API Endpoints Tests")
class Cbcus01cControllerTest {

    private Cbcus01cService service;
    private Cbcus01cController controller;

    @BeforeEach
    void setUp() {
        service = new Cbcus01cService();
        controller = new Cbcus01cController(service);
    }

    @Test
    @DisplayName("GET /api/v1/core_financial_processing/cbcus01c/health returns HTTP 200 and status ACTIVE")
    void testHealthEndpoint() {
        ResponseEntity<Map<String, Object>> response = controller.getHealth();
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertEquals("ACTIVE", response.getBody().get("status"));
        assertEquals("CBCUS01C", response.getBody().get("moduleId"));
    }

    @Test
    @DisplayName("POST /api/v1/core_financial_processing/cbcus01c/cbcus01c0031 returns HTTP 200")
    void testActionEndpoint_Success() {
        Cbcus01cRequest request = new Cbcus01cRequest(
            "00000000001",
            new BigDecimal("150.00"),
            "DEBIT",
            "TXN-1001",
            Map.of()
        );
        ResponseEntity<Cbcus01cResponse> response = controller.handleCbcus01c0031(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertTrue(response.getBody().status().equals("SUCCESS") || response.getBody().status().equals("UNTRANSLATED"));
    }
}
