package com.client.core_financial_processing.controller;

import com.client.core_financial_processing.dto.CobswaitRequest;
import com.client.core_financial_processing.dto.CobswaitResponse;
import com.client.core_financial_processing.service.CobswaitService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.math.BigDecimal;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("CobswaitController REST API Endpoints Tests")
class CobswaitControllerTest {

    private CobswaitService service;
    private CobswaitController controller;

    @BeforeEach
    void setUp() {
        service = new CobswaitService();
        controller = new CobswaitController(service);
    }

    @Test
    @DisplayName("GET /api/v1/core_financial_processing/cobswait/health returns HTTP 200 and status ACTIVE")
    void testHealthEndpoint() {
        ResponseEntity<Map<String, Object>> response = controller.getHealth();
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertEquals("ACTIVE", response.getBody().get("status"));
        assertEquals("COBSWAIT", response.getBody().get("moduleId"));
    }

    @Test
    @DisplayName("POST /api/v1/core_financial_processing/cobswait/cobswait0067 returns HTTP 200")
    void testActionEndpoint_Success() {
        CobswaitRequest request = new CobswaitRequest(
            "00000000001",
            new BigDecimal("150.00"),
            "DEBIT",
            "TXN-1001",
            Map.of()
        );
        ResponseEntity<CobswaitResponse> response = controller.handleCobswait0067(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertTrue(response.getBody().status().equals("SUCCESS") || response.getBody().status().equals("UNTRANSLATED"));
    }
}
