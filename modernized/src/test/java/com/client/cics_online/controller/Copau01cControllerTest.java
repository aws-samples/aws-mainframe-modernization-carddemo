package com.client.cics_online.controller;

import com.client.cics_online.dto.Copau01cRequest;
import com.client.cics_online.dto.Copau01cResponse;
import com.client.cics_online.service.Copau01cService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.math.BigDecimal;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("Copau01cController REST API Endpoints Tests")
class Copau01cControllerTest {

    private Copau01cService service;
    private Copau01cController controller;

    @BeforeEach
    void setUp() {
        service = new Copau01cService();
        controller = new Copau01cController(service);
    }

    @Test
    @DisplayName("GET /api/v1/cics_online/copau01c/health returns HTTP 200 and status ACTIVE")
    void testHealthEndpoint() {
        ResponseEntity<Map<String, Object>> response = controller.getHealth();
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertEquals("ACTIVE", response.getBody().get("status"));
        assertEquals("COPAU01C", response.getBody().get("moduleId"));
    }

    @Test
    @DisplayName("POST /api/v1/cics_online/copau01c/copau01c001 returns HTTP 200")
    void testActionEndpoint_Success() {
        Copau01cRequest request = new Copau01cRequest(
            "00000000001",
            new BigDecimal("150.00"),
            "DEBIT",
            "TXN-1001",
            Map.of()
        );
        ResponseEntity<Copau01cResponse> response = controller.handleCopau01c001(request);
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertTrue(response.getBody().status().equals("SUCCESS") || response.getBody().status().equals("UNTRANSLATED"));
    }
}
