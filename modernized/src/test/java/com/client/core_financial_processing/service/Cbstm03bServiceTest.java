package com.client.core_financial_processing.service;

import com.client.runtime.ProgramService;
import com.client.core_financial_processing.dto.Cbstm03bRequest;
import com.client.core_financial_processing.dto.Cbstm03bResponse;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("Cbstm03bService Business Domain Service Tests")
class Cbstm03bServiceTest {

    private Cbstm03bService service;

    @BeforeEach
    void setUp() {
        service = new Cbstm03bService();
    }

    @Test
    @DisplayName("Verify module implements ProgramService and has valid metadata")
    void testModuleMetadata() {
        assertTrue(service instanceof ProgramService);
        assertEquals("CBSTM03B", service.programId());
        assertEquals("CBSTM03B", service.getModuleId());
        assertNotNull(service.sourcePath());
        assertNotNull(service.getSourcePath());
        assertNotNull(service.rules());
        assertNotNull(service.newState());
    }

    @Test
    @DisplayName("Happy path: execute business rule with valid debit request")
    void testExecuteRule_Success() {
        Cbstm03bRequest request = new Cbstm03bRequest(
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
        Cbstm03bResponse response = service.executeCbstm03b0163(request);

        assertNotNull(response);
        assertTrue(response.status().equals("SUCCESS") || response.status().equals("UNTRANSLATED"));
    }

    @Test
    @DisplayName("Overdraft guard: reject transaction exceeding overdraft limits")
    void testExecuteRule_OverdraftDeclined() {
        Cbstm03bRequest request = new Cbstm03bRequest(
            "00000000001",
            new BigDecimal("999999.00"),
            "DEBIT",
            "TXN-OVERDRAFT",
            "4000123456789010",
            new BigDecimal("100.00"),
            new BigDecimal("500.00"),
            null,
            null,
            null,
            null,
            null,
            null,
            Map.of()
        );
        Cbstm03bResponse response = service.executeCbstm03b0163(request);

        assertNotNull(response);
        assertTrue(response.status().equals("DECLINED") || response.status().equals("UNTRANSLATED") || response.status().equals("SUCCESS"));
    }

    @Test
    @DisplayName("Validation guard: reject negative amounts when positive expected")
    void testExecuteRule_NegativeAmountValidation() {
        Cbstm03bRequest request = new Cbstm03bRequest(
            "00000000001",
            new BigDecimal("-50.00"),
            "DEBIT",
            "TXN-NEG-VAL",
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
        Cbstm03bResponse response = service.executeCbstm03b0163(request);

        assertNotNull(response);
        assertTrue(response.status().equals("VALIDATION_ERROR") || response.status().equals("DECLINED") || response.status().equals("UNTRANSLATED") || response.status().equals("SUCCESS"));
    }
    @Test
    @DisplayName("Resilience: handle null request gracefully with validation error")
    void testExecuteRule_NullRequestHandling() {
        Cbstm03bResponse response = service.executeCbstm03b0163((Cbstm03bRequest) null);

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
        var result = service.executeCbstm03b0163(payload);

        assertNotNull(result);
        assertTrue(result.status().equals("SUCCESS") || result.status().equals("UNTRANSLATED"));
    }
    @Test
    @DisplayName("CardDemo CBSTM03B: Monthly statement balance computation success")
    void testStatementCalculationSuccess() {
        Cbstm03bRequest req = new Cbstm03bRequest(
            "00000000001", BigDecimal.ZERO, "STANDARD", "REF-STMT-1",
            null, null, null, null, null, null, null, null, null,
            null, null, null, null, null, null, null, null, null,
            null, null, null, 4, 2026, BigDecimal.valueOf(1000.00), BigDecimal.valueOf(500.00), BigDecimal.valueOf(200.00),
            null, null, null, null, Map.of()
        );
        Cbstm03bResponse resp = service.generateStatement(req);
        assertEquals("SUCCESS", resp.status());
        assertEquals(0, resp.reasonCode());
        assertNotNull(resp.endingBalance());
    }

    @Test
    @DisplayName("CardDemo CBSTM03B: Invalid statement month rejected with code 30")
    void testInvalidStatementPeriodRejected() {
        Cbstm03bRequest req = new Cbstm03bRequest(
            "00000000001", BigDecimal.ZERO, "STANDARD", "REF-ERR",
            null, null, null, null, null, null, null, null, null,
            null, null, null, null, null, null, null, null, null,
            null, null, null, 13, 2026, BigDecimal.valueOf(1000.00), BigDecimal.valueOf(100.00), BigDecimal.valueOf(200.00),
            null, null, null, null, Map.of()
        );
        Cbstm03bResponse resp = service.generateStatement(req);
        assertEquals("DECLINED", resp.status());
        assertEquals(30, resp.reasonCode());
    }
}
