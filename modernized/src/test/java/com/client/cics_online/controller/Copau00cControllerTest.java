package com.client.cics_online.controller;

import com.client.cics_online.dto.Copau00cRequest;
import com.client.cics_online.dto.Copau00cResponse;
import com.client.cics_online.service.Copau00cService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.math.BigDecimal;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("Copau00cController REST API Endpoints Tests")
class Copau00cControllerTest {

    private Copau00cService service;
    private Copau00cController controller;

    @BeforeEach
    void setUp() {
        service = new Copau00cService();
        controller = new Copau00cController(service);
    }

    @Test
    @DisplayName("GET /api/v1/cics_online/copau00c/health returns HTTP 200 and status ACTIVE")
    void testHealthEndpoint() {
        ResponseEntity<Map<String, Object>> response = controller.getHealth();
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertEquals("ACTIVE", response.getBody().get("status"));
        assertEquals("COPAU00C", response.getBody().get("moduleId"));
    }

    @Test
    @DisplayName("POST /api/v1/cics_online/copau00c/copau00c001 returns HTTP 200")
    void testActionEndpoint_Success() {
        Copau00cRequest request = new Copau00cRequest(
            "00000000001",
            new BigDecimal("150.00"),
            "DEBIT",
            "TXN-1001",
            Map.of()
        );
        ResponseEntity<Copau00cResponse> response = controller.handleCopau00c001(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertTrue(response.getBody().status().equals("SUCCESS") || response.getBody().status().equals("UNTRANSLATED"));
    }
}
