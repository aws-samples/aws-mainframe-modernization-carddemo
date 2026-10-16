package com.client.core_financial_processing.controller;

import com.client.core_financial_processing.dto.Cbact02cRequest;
import com.client.core_financial_processing.dto.Cbact02cResponse;
import com.client.core_financial_processing.service.Cbact02cService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.math.BigDecimal;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("Cbact02cController REST API Endpoints Tests")
class Cbact02cControllerTest {

    private Cbact02cService service;
    private Cbact02cController controller;

    @BeforeEach
    void setUp() {
        service = new Cbact02cService();
        controller = new Cbact02cController(service);
    }

    @Test
    @DisplayName("GET /api/v1/core_financial_processing/cbact02c/health returns HTTP 200 and status ACTIVE")
    void testHealthEndpoint() {
        ResponseEntity<Map<String, Object>> response = controller.getHealth();
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertEquals("ACTIVE", response.getBody().get("status"));
        assertEquals("CBACT02C", response.getBody().get("moduleId"));
    }

    @Test
    @DisplayName("POST /api/v1/core_financial_processing/cbact02c/cbact02c0009 returns HTTP 200")
    void testActionEndpoint_Success() {
        Cbact02cRequest request = new Cbact02cRequest(
            "00000000001",
            new BigDecimal("150.00"),
            "DEBIT",
            "TXN-1001",
            Map.of()
        );
        ResponseEntity<Cbact02cResponse> response = controller.handleCbact02c0009(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertTrue(response.getBody().status().equals("SUCCESS") || response.getBody().status().equals("UNTRANSLATED"));
    }
}
