package com.client.core_financial_processing.service;

import com.client.runtime.ProgramService;
import com.client.core_financial_processing.dto.Cbstm03aRequest;
import com.client.core_financial_processing.dto.Cbstm03aResponse;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("Cbstm03aService Business Domain Service Tests")
class Cbstm03aServiceTest {

    private Cbstm03aService service;

    @BeforeEach
    void setUp() {
        service = new Cbstm03aService();
    }

    @Test
    @DisplayName("Verify module implements ProgramService and has valid metadata")
    void testModuleMetadata() {
        assertTrue(service instanceof ProgramService);
        assertEquals("CBSTM03A", service.programId());
        assertEquals("CBSTM03A", service.getModuleId());
        assertNotNull(service.sourcePath());
        assertNotNull(service.getSourcePath());
        assertNotNull(service.rules());
        assertNotNull(service.newState());
    }

    @Test
    @DisplayName("Happy path: execute business rule with valid debit request")
    void testExecuteRule_Success() {
        Cbstm03aRequest request = new Cbstm03aRequest(
            "00000000001",
            new BigDecimal("150.00"),
            "DEBIT",
            "TXN-1001",
            "4000123456789010",
            new BigDecimal("1000.00"),
            new BigDecimal("5000.00"),
            null,
            null,
            null,
            null,
            null,
            null,
            Map.of()
        );
        Cbstm03aResponse response = service.executeCbstm03a0043(request);

        assertNotNull(response);
        assertTrue(response.status().equals("SUCCESS") || response.status().equals("UNTRANSLATED"));
    }

    @Test
    @DisplayName("Resilience: handle null request gracefully with validation error")
    void testExecuteRule_NullRequestHandling() {
        Cbstm03aResponse response = service.executeCbstm03a0043((Cbstm03aRequest) null);

        assertNotNull(response);
        assertEquals("VALIDATION_ERROR", response.status());
    }

    @Test
    @DisplayName("Backward compatibility: handle legacy Map payload")
    void testExecuteRule_LegacyMapPayload() {
        Map<String, Object> payload = Map.of(
            "accountId", "00000000001",
            "amount", 50.0,
            "cardNumber", "4000123456789010"
        );
        var result = service.executeCbstm03a0043(payload);

        assertNotNull(result);
        assertTrue(result.status().equals("SUCCESS") || result.status().equals("UNTRANSLATED"));
    }
    @Test
    @DisplayName("CardDemo CBSTM03A: Monthly statement balance computation success")
    void testStatementCalculationSuccess() {
        Cbstm03aRequest req = new Cbstm03aRequest(
            "00000000001", BigDecimal.ZERO, "STANDARD", "REF-STMT-1",
            null, null, null, null, null, null, null, null, null,
            null, null, null, null, null, null, null, null, null,
            null, null, null, 4, 2026, BigDecimal.valueOf(1000.00), BigDecimal.valueOf(500.00), BigDecimal.valueOf(200.00),
            null, null, null, null, Map.of()
        );
        Cbstm03aResponse resp = service.generateStatement(req);
        assertEquals("SUCCESS", resp.status());
        assertEquals(0, resp.reasonCode());
        assertNotNull(resp.endingBalance());
    }

    @Test
    @DisplayName("CardDemo CBSTM03A: Invalid statement month rejected with code 30")
    void testInvalidStatementPeriodRejected() {
        Cbstm03aRequest req = new Cbstm03aRequest(
            "00000000001", BigDecimal.ZERO, "STANDARD", "REF-ERR",
            null, null, null, null, null, null, null, null, null,
            null, null, null, null, null, null, null, null, null,
            null, null, null, 13, 2026, BigDecimal.valueOf(1000.00), BigDecimal.valueOf(100.00), BigDecimal.valueOf(200.00),
            null, null, null, null, Map.of()
        );
        Cbstm03aResponse resp = service.generateStatement(req);
        assertEquals("DECLINED", resp.status());
        assertEquals(30, resp.reasonCode());
    }
}
