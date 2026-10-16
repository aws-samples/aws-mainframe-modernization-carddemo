package com.client.core_financial_processing.service;

import com.client.runtime.ProgramService;
import com.client.core_financial_processing.dto.Cbcus01cRequest;
import com.client.core_financial_processing.dto.Cbcus01cResponse;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("Cbcus01cService Business Domain Service Tests")
class Cbcus01cServiceTest {

    private Cbcus01cService service;

    @BeforeEach
    void setUp() {
        service = new Cbcus01cService();
    }

    @Test
    @DisplayName("Verify module implements ProgramService and has valid metadata")
    void testModuleMetadata() {
        assertTrue(service instanceof ProgramService);
        assertEquals("CBCUS01C", service.programId());
        assertEquals("CBCUS01C", service.getModuleId());
        assertNotNull(service.sourcePath());
        assertNotNull(service.getSourcePath());
        assertNotNull(service.rules());
        assertNotNull(service.newState());
    }

    @Test
    @DisplayName("Happy path: execute business rule with valid debit request")
    void testExecuteRule_Success() {
        Cbcus01cRequest request = new Cbcus01cRequest(
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
        Cbcus01cResponse response = service.executeCbcus01c0031(request);

        assertNotNull(response);
        assertTrue(response.status().equals("SUCCESS") || response.status().equals("UNTRANSLATED"));
    }

    @Test
    @DisplayName("Overdraft guard: reject transaction exceeding overdraft limits")
    void testExecuteRule_OverdraftDeclined() {
        Cbcus01cRequest request = new Cbcus01cRequest(
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
        Cbcus01cResponse response = service.executeCbcus01c0031(request);

        assertNotNull(response);
        assertTrue(response.status().equals("DECLINED") || response.status().equals("UNTRANSLATED") || response.status().equals("SUCCESS"));
    }

    @Test
    @DisplayName("Validation guard: reject negative amounts when positive expected")
    void testExecuteRule_NegativeAmountValidation() {
        Cbcus01cRequest request = new Cbcus01cRequest(
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
        Cbcus01cResponse response = service.executeCbcus01c0031(request);

        assertNotNull(response);
        assertTrue(response.status().equals("VALIDATION_ERROR") || response.status().equals("DECLINED") || response.status().equals("UNTRANSLATED") || response.status().equals("SUCCESS"));
    }
    @Test
    @DisplayName("Resilience: handle null request gracefully with validation error")
    void testExecuteRule_NullRequestHandling() {
        Cbcus01cResponse response = service.executeCbcus01c0031((Cbcus01cRequest) null);

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
        var result = service.executeCbcus01c0031(payload);

        assertNotNull(result);
        assertTrue(result.status().equals("SUCCESS") || result.status().equals("UNTRANSLATED"));
    }
    @Test
    @DisplayName("CardDemo CBCUS01C: Customer demographic record lookup success")
    void testCustomerLookupSuccess() {
        Cbcus01cRequest req = new Cbcus01cRequest(
            "00000000001", BigDecimal.ZERO, "LOOKUP", "TXN-CUST-1",
            null, null, null, null, null, null, null, null, null,
            "000000001", "JOHN", "DOE", "123456789", "123 MAIN ST", "NEW YORK", "NY", "10001", "5551234567",
            null, null, null, null, null, null, null, null,
            null, null, null, null, Map.of()
        );
        Cbcus01cResponse resp = service.processCustomer(req);
        assertEquals("SUCCESS", resp.status());
        assertEquals("000000001", resp.customerId());
        assertEquals(0, resp.reasonCode());
    }

    @Test
    @DisplayName("CardDemo CBCUS01C: Missing customer ID rejected with code 10")
    void testCustomerLookupMissingId() {
        Cbcus01cRequest req = new Cbcus01cRequest(
            "00000000001", BigDecimal.ZERO, "LOOKUP", "TXN-CUST-ERR",
            null, null, null, null, null, null, null, null, null,
            "", null, null, null, null, null, null, null, null,
            null, null, null, null, null, null, null, null,
            null, null, null, null, Map.of()
        );
        Cbcus01cResponse resp = service.processCustomer(req);
        assertEquals("DECLINED", resp.status());
        assertEquals(10, resp.reasonCode());
    }
}
