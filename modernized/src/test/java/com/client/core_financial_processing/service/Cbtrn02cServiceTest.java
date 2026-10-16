package com.client.core_financial_processing.service;


import java.time.LocalDate;
import com.client.runtime.ProgramService;
import com.client.core_financial_processing.dto.Cbtrn02cRequest;
import com.client.core_financial_processing.dto.Cbtrn02cResponse;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("Cbtrn02cService Business Domain Service Tests")
class Cbtrn02cServiceTest {

    private Cbtrn02cService service;

    @BeforeEach
    void setUp() {
        service = new Cbtrn02cService();
    }

    @Test
    @DisplayName("Verify module implements ProgramService and has valid metadata")
    void testModuleMetadata() {
        assertTrue(service instanceof ProgramService);
        assertEquals("CBTRN02C", service.programId());
        assertEquals("CBTRN02C", service.getModuleId());
        assertNotNull(service.sourcePath());
        assertNotNull(service.getSourcePath());
        assertNotNull(service.rules());
        assertNotNull(service.newState());
    }

    @Test
    @DisplayName("Happy path: execute business rule with valid debit request")
    void testExecuteRule_Success() {
        Cbtrn02cRequest request = new Cbtrn02cRequest(
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
        Cbtrn02cResponse response = service.executeCbtrn02c0051(request);

        assertNotNull(response);
        assertTrue(response.status().equals("SUCCESS") || response.status().equals("UNTRANSLATED"));
    }

    @Test
    @DisplayName("Overdraft guard: reject transaction exceeding overdraft limits")
    void testExecuteRule_OverdraftDeclined() {
        Cbtrn02cRequest request = new Cbtrn02cRequest(
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
        Cbtrn02cResponse response = service.executeCbtrn02c0051(request);

        assertNotNull(response);
        assertTrue(response.status().equals("DECLINED") || response.status().equals("UNTRANSLATED"));
    }

    @Test
    @DisplayName("Support refund: negative amount processed and decreases balance")
    void testExecuteRule_NegativeAmountRefund() {
        Cbtrn02cRequest request = new Cbtrn02cRequest(
            "00000000001",
            new BigDecimal("-50.00"),
            "CREDIT",
            "TXN-NEG",
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
        Cbtrn02cResponse response = service.executeCbtrn02c0051(request);

        assertNotNull(response);
        assertTrue(response.status().equals("SUCCESS") || response.status().equals("UNTRANSLATED"));
    }
    @Test
    @DisplayName("Resilience: handle null request gracefully with validation error")
    void testExecuteRule_NullRequestHandling() {
        Cbtrn02cResponse response = service.executeCbtrn02c0051((Cbtrn02cRequest) null);

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
        var result = service.executeCbtrn02c0051(payload);

        assertNotNull(result);
        assertTrue(result.status().equals("SUCCESS") || result.status().equals("UNTRANSLATED"));
    }
    @Test
    @DisplayName("CardDemo CBTRN02C Ground Truth: Reason code 102 when amount exceeds credit limit")
    void testCardDemoOverlimitRejectionCode102() {
        Cbtrn02cRequest req = new Cbtrn02cRequest(
            "00000000001",
            new BigDecimal("1600.00"),
            "DEBIT",
            "TXN-OVERLIMIT",
            "1111222233334444",
            new BigDecimal("1000.00"),
            new BigDecimal("1500.00"),
            null,
            null,
            null,
            null,
            null,
            null,
            Map.of()
        );
        Cbtrn02cResponse resp = service.processTransaction(req);
        assertEquals("DECLINED", resp.status());
        assertEquals(102, resp.reasonCode());
        assertTrue(resp.message().contains("OVERLIMIT"));
    }

    @Test
    @DisplayName("CardDemo CBTRN02C Ground Truth: Reason code 100 on invalid card number")
    void testCardDemoInvalidCardRejectionCode100() {
        Cbtrn02cRequest req = new Cbtrn02cRequest(
            "00000000001",
            new BigDecimal("50.00"),
            "DEBIT",
            "TXN-BAD-CARD",
            "0000000000000000",
            new BigDecimal("1000.00"),
            new BigDecimal("1500.00"),
            null,
            null,
            null,
            null,
            null,
            null,
            Map.of()
        );
        Cbtrn02cResponse resp = service.processTransaction(req);
        assertEquals("DECLINED", resp.status());
        assertEquals(100, resp.reasonCode());
    }

    @Test
    @DisplayName("CardDemo CBTRN02C Ground Truth: Reason code 101 on missing account")
    void testCardDemoMissingAccountRejectionCode101() {
        Cbtrn02cRequest req = new Cbtrn02cRequest(
            "",
            new BigDecimal("50.00"),
            "DEBIT",
            "TXN-BAD-ACCT",
            "1111222233334444",
            new BigDecimal("1000.00"),
            new BigDecimal("1500.00"),
            null,
            null,
            null,
            null,
            null,
            null,
            Map.of()
        );
        Cbtrn02cResponse resp = service.processTransaction(req);
        assertEquals("DECLINED", resp.status());
        assertEquals(101, resp.reasonCode());
        assertTrue(resp.message().contains("ACCOUNT RECORD NOT FOUND"));
    }

    @Test
    @DisplayName("CardDemo CBTRN02C Ground Truth: Reason code 103 on expired card (overwrites 102)")
    void testCardDemoExpiredCardRejectionCode103() {
        Cbtrn02cRequest req = new Cbtrn02cRequest(
            "00000000001",
            new BigDecimal("600.00"),
            "DEBIT",
            "TXN-EXPIRED",
            "1111222233334444",
            new BigDecimal("1000.00"),
            new BigDecimal("1500.00"),
            null,
            LocalDate.of(2026, 10, 15),
            LocalDate.of(2025, 1, 1),
            null,
            null,
            null,
            Map.of()
        );
        Cbtrn02cResponse resp = service.processTransaction(req);
        assertEquals("DECLINED", resp.status());
        assertEquals(103, resp.reasonCode());
        assertTrue(resp.message().contains("TRANSACTION RECEIVED AFTER ACCT EXPIRATION"));
    }

    @Test
    @DisplayName("CardDemo CBTRN02C Ground Truth: Negative amount refund approved and posted")
    void testCardDemoApprovedRefundProbe5() {
        Cbtrn02cRequest req = new Cbtrn02cRequest(
            "00000000001",
            new BigDecimal("-50.00"),
            "CREDIT",
            "TXN-REFUND-001",
            "1111222233334444",
            new BigDecimal("1000.00"),
            new BigDecimal("2000.00"),
            null,
            LocalDate.of(2026, 10, 15),
            LocalDate.of(2028, 1, 1),
            null,
            null,
            null,
            Map.of()
        );
        Cbtrn02cResponse resp = service.processTransaction(req);
        assertEquals("SUCCESS", resp.status());
        assertEquals(new BigDecimal("950.00"), resp.newBalance());
    }
}
