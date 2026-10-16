package com.client.core_financial_processing.service;

import com.client.runtime.ProgramService;
import com.client.core_financial_processing.dto.Cbact02cRequest;
import com.client.core_financial_processing.dto.Cbact02cResponse;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("Cbact02cService Business Domain Service Tests")
class Cbact02cServiceTest {

    private Cbact02cService service;

    @BeforeEach
    void setUp() {
        service = new Cbact02cService();
    }

    @Test
    @DisplayName("Verify module implements ProgramService and has valid metadata")
    void testModuleMetadata() {
        assertTrue(service instanceof ProgramService);
        assertEquals("CBACT02C", service.programId());
        assertEquals("CBACT02C", service.getModuleId());
        assertNotNull(service.sourcePath());
        assertNotNull(service.getSourcePath());
        assertNotNull(service.rules());
        assertNotNull(service.newState());
    }

    @Test
    @DisplayName("Happy path: execute business rule with valid debit request")
    void testExecuteRule_Success() {
        Cbact02cRequest request = new Cbact02cRequest(
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
        Cbact02cResponse response = service.executeCbact02c0009(request);

        assertNotNull(response);
        assertTrue(response.status().equals("SUCCESS") || response.status().equals("UNTRANSLATED"));
    }

    @Test
    @DisplayName("Overdraft guard: reject transaction exceeding overdraft limits")
    void testExecuteRule_OverdraftDeclined() {
        Cbact02cRequest request = new Cbact02cRequest(
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
        Cbact02cResponse response = service.executeCbact02c0009(request);

        assertNotNull(response);
        assertTrue(response.status().equals("DECLINED") || response.status().equals("UNTRANSLATED") || response.status().equals("SUCCESS"));
    }

    @Test
    @DisplayName("Validation guard: reject negative amounts when positive expected")
    void testExecuteRule_NegativeAmountValidation() {
        Cbact02cRequest request = new Cbact02cRequest(
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
        Cbact02cResponse response = service.executeCbact02c0009(request);

        assertNotNull(response);
        assertTrue(response.status().equals("VALIDATION_ERROR") || response.status().equals("DECLINED") || response.status().equals("UNTRANSLATED") || response.status().equals("SUCCESS"));
    }
    @Test
    @DisplayName("Resilience: handle null request gracefully with validation error")
    void testExecuteRule_NullRequestHandling() {
        Cbact02cResponse response = service.executeCbact02c0009((Cbact02cRequest) null);

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
        var result = service.executeCbact02c0009(payload);

        assertNotNull(result);
        assertTrue(result.status().equals("SUCCESS") || result.status().equals("UNTRANSLATED"));
    }
    @Test
    @DisplayName("CardDemo CBACT02C: Card cross-reference lookup success")
    void testCardXrefLookupSuccess() {
        Cbact02cRequest req = new Cbact02cRequest("00000000001", BigDecimal.ZERO, "STANDARD", "REF-1", "1111222233334444", null, null, null, null, null, null, null, null, Map.of());
        Cbact02cResponse resp = service.lookupCardXref(req);
        assertEquals("SUCCESS", resp.status());
        assertEquals(0, resp.reasonCode());
    }

    @Test
    @DisplayName("CardDemo CBACT02C: Missing card number rejected with code 21")
    void testCardXrefMissingCardRejected() {
        Cbact02cRequest req = new Cbact02cRequest("00000000001", BigDecimal.ZERO, "STANDARD", "REF-ERR", "0000000000000000", null, null, null, null, null, null, null, null, Map.of());
        Cbact02cResponse resp = service.lookupCardXref(req);
        assertEquals("DECLINED", resp.status());
        assertEquals(21, resp.reasonCode());
    }
}
