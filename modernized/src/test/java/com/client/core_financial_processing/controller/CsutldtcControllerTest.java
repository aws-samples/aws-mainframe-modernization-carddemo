package com.client.core_financial_processing.controller;

import com.client.core_financial_processing.dto.CsutldtcRequest;
import com.client.core_financial_processing.dto.CsutldtcResponse;
import com.client.core_financial_processing.service.CsutldtcService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.math.BigDecimal;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("CsutldtcController REST API Endpoints Tests")
class CsutldtcControllerTest {

    private CsutldtcService service;
    private CsutldtcController controller;

    @BeforeEach
    void setUp() {
        service = new CsutldtcService();
        controller = new CsutldtcController(service);
    }

    @Test
    @DisplayName("GET /api/v1/core_financial_processing/csutldtc/health returns HTTP 200 and status ACTIVE")
    void testHealthEndpoint() {
        ResponseEntity<Map<String, Object>> response = controller.getHealth();
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertEquals("ACTIVE", response.getBody().get("status"));
        assertEquals("CSUTLDTC", response.getBody().get("moduleId"));
    }

    @Test
    @DisplayName("POST /api/v1/core_financial_processing/csutldtc/csutldtc0283 returns HTTP 200")
    void testActionEndpoint_Success() {
        CsutldtcRequest request = new CsutldtcRequest(
            "00000000001",
            new BigDecimal("150.00"),
            "DEBIT",
            "TXN-1001",
            Map.of()
        );
        ResponseEntity<CsutldtcResponse> response = controller.handleCsutldtc0283(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertTrue(response.getBody().status().equals("SUCCESS") || response.getBody().status().equals("UNTRANSLATED"));
    }
}
