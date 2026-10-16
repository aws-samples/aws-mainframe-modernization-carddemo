package com.client.core_financial_processing.controller;

import com.client.core_financial_processing.dto.Cbact03cRequest;
import com.client.core_financial_processing.dto.Cbact03cResponse;
import com.client.core_financial_processing.service.Cbact03cService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.math.BigDecimal;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("Cbact03cController REST API Endpoints Tests")
class Cbact03cControllerTest {

    private Cbact03cService service;
    private Cbact03cController controller;

    @BeforeEach
    void setUp() {
        service = new Cbact03cService();
        controller = new Cbact03cController(service);
    }

    @Test
    @DisplayName("GET /api/v1/core_financial_processing/cbact03c/health returns HTTP 200 and status ACTIVE")
    void testHealthEndpoint() {
        ResponseEntity<Map<String, Object>> response = controller.getHealth();
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertEquals("ACTIVE", response.getBody().get("status"));
        assertEquals("CBACT03C", response.getBody().get("moduleId"));
    }

    @Test
    @DisplayName("POST /api/v1/core_financial_processing/cbact03c/cbact03c0016 returns HTTP 200")
    void testActionEndpoint_Success() {
        Cbact03cRequest request = new Cbact03cRequest(
            "00000000001",
            new BigDecimal("150.00"),
            "DEBIT",
            "TXN-1001",
            Map.of()
        );
        ResponseEntity<Cbact03cResponse> response = controller.handleCbact03c0016(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertTrue(response.getBody().status().equals("SUCCESS") || response.getBody().status().equals("UNTRANSLATED"));
    }
}
