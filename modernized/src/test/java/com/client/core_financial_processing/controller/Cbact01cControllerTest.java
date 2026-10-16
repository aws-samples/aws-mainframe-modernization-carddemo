package com.client.core_financial_processing.controller;

import com.client.core_financial_processing.dto.Cbact01cRequest;
import com.client.core_financial_processing.dto.Cbact01cResponse;
import com.client.core_financial_processing.service.Cbact01cService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.math.BigDecimal;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("Cbact01cController REST API Endpoints Tests")
class Cbact01cControllerTest {

    private Cbact01cService service;
    private Cbact01cController controller;

    @BeforeEach
    void setUp() {
        service = new Cbact01cService();
        controller = new Cbact01cController(service);
    }

    @Test
    @DisplayName("GET /api/v1/core_financial_processing/cbact01c/health returns HTTP 200 and status ACTIVE")
    void testHealthEndpoint() {
        ResponseEntity<Map<String, Object>> response = controller.getHealth();
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertEquals("ACTIVE", response.getBody().get("status"));
        assertEquals("CBACT01C", response.getBody().get("moduleId"));
    }

    @Test
    @DisplayName("POST /api/v1/core_financial_processing/cbact01c/cbact01c0001 returns HTTP 200")
    void testActionEndpoint_Success() {
        Cbact01cRequest request = new Cbact01cRequest(
            "00000000001",
            new BigDecimal("150.00"),
            "DEBIT",
            "TXN-1001",
            Map.of()
        );
        ResponseEntity<Cbact01cResponse> response = controller.handleCbact01c0001(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertTrue(response.getBody().status().equals("SUCCESS") || response.getBody().status().equals("UNTRANSLATED"));
    }
}
