package com.client.core_financial_processing.controller;

import com.client.core_financial_processing.dto.Cbact04cRequest;
import com.client.core_financial_processing.dto.Cbact04cResponse;
import com.client.core_financial_processing.service.Cbact04cService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.math.BigDecimal;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("Cbact04cController REST API Endpoints Tests")
class Cbact04cControllerTest {

    private Cbact04cService service;
    private Cbact04cController controller;

    @BeforeEach
    void setUp() {
        service = new Cbact04cService();
        controller = new Cbact04cController(service);
    }

    @Test
    @DisplayName("GET /api/v1/core_financial_processing/cbact04c/health returns HTTP 200 and status ACTIVE")
    void testHealthEndpoint() {
        ResponseEntity<Map<String, Object>> response = controller.getHealth();
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertEquals("ACTIVE", response.getBody().get("status"));
        assertEquals("CBACT04C", response.getBody().get("moduleId"));
    }

    @Test
    @DisplayName("POST /api/v1/core_financial_processing/cbact04c/cbact04c0023 returns HTTP 200")
    void testActionEndpoint_Success() {
        Cbact04cRequest request = new Cbact04cRequest(
            "00000000001",
            new BigDecimal("150.00"),
            "DEBIT",
            "TXN-1001",
            Map.of()
        );
        ResponseEntity<Cbact04cResponse> response = controller.handleCbact04c0023(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertTrue(response.getBody().status().equals("SUCCESS") || response.getBody().status().equals("UNTRANSLATED"));
    }
}
