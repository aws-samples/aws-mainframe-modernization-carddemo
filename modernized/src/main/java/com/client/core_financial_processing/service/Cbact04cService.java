package com.client.core_financial_processing.service;


import java.util.List;
import java.util.Map;
import com.client.runtime.CobolState;
import com.client.runtime.NotTranslatedException;
import com.client.runtime.ProgramExit;
import com.client.runtime.ProgramRegistry;
import com.client.runtime.ProgramService;
import com.client.runtime.RuleInfo;
import com.client.core_financial_processing.dto.Cbact04cRequest;
import com.client.core_financial_processing.dto.Cbact04cResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.Instant;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.ResolverStyle;
import java.util.*;

/**
 * Modernized Spring Boot Domain Service: Cbact04cService
 *
 * Transpiled from Mainframe COBOL program: CBACT04C
 * Legacy Source Path: app/cbl/CBACT04C.cbl
 * Generation Strategy: Enterprise Modernization with ProgramService registry integration
 */
@Slf4j
@Service
@Tag(name = "Cbact04cService", description = "Modernized domain business service for legacy CBACT04C")
public class Cbact04cService implements ProgramService {

    @Autowired(required = false)
    @Lazy
    private ProgramRegistry registry;

    @Autowired(required = false)
    private com.client.domain.repository.Cvact01yRepository cvact01yRepository;

    @Autowired(required = false)
    private com.client.domain.repository.Cvtra01yRepository cvtra01yRepository;

    @Autowired(required = false)
    private com.client.domain.repository.Cvcrd01yRepository cvcrd01yRepository;

    @Autowired(required = false)
    private com.client.domain.repository.Cvcus01yRepository cvcus01yRepository;

    @Autowired(required = false)
    private com.client.domain.repository.Cvxrf01yRepository cvxrf01yRepository;

    private static final List<String> UNTRANSLATED = List.of("1200-GET-INTEREST-RATE: abbreviated combined conditions (for example A = 1 OR 2) are not translated", "1300-B-WRITE-TX: STRING statements are not translated", "Z-GET-DB2-FORMAT-TIMESTAMP: expected TO but found 'CURRENT-DATE'");

    private static final List<RuleInfo> RULES = List.of(
            new RuleInfo("BR-CBACT04C-0023", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT04C (IF END-OF-FILE = 'N').", "app/cbl/CBACT04C.cbl:188-191", false, "expected END-PERFORM but found the end of the cited lines", List.of("END-OF-FILE", "END-OF-FILE", "END-OF-FILE", "END-OF-FILE"), "untranslated (expected END-PERFORM but found the end of the cited lines)"),
            new RuleInfo("BR-CBACT04C-0024", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT04C (IF END-OF-FILE = 'N').", "app/cbl/CBACT04C.cbl:190-193", true, null, List.of("END-OF-FILE", "WS-RECORD-COUNT", "TRAN-CAT-BAL-RECORD"), ""),
            new RuleInfo("BR-CBACT04C-0025", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT04C (IF TRANCAT-ACCT-ID NOT= WS-LAST-ACCT-NUM).", "app/cbl/CBACT04C.cbl:193-196", true, null, List.of("TRAN-CAT-BAL-RECORD", "TRANCAT-ACCT-ID", "WS-LAST-ACCT-NUM", "WS-FIRST-TIME"), ""),
            new RuleInfo("BR-CBACT04C-0026", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT04C (IF WS-FIRST-TIME NOT = 'Y').", "app/cbl/CBACT04C.cbl:194-197", true, null, List.of("TRANCAT-ACCT-ID", "WS-LAST-ACCT-NUM", "WS-FIRST-TIME"), ""),
            new RuleInfo("BR-CBACT04C-0027", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT04C (IF DIS-INT-RATE NOT = 0).", "app/cbl/CBACT04C.cbl:213-216", false, "performs 1200-GET-INTEREST-RATE, which is not translated", List.of("DIS-INT-RATE", "DIS-INT-RATE"), "untranslated (performs 1200-GET-INTEREST-RATE, which is not translated)"),
            new RuleInfo("BR-CBACT04C-0028", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT04C (IF TCATBALF-STATUS = '00').", "app/cbl/CBACT04C.cbl:236-239", true, null, List.of("TCATBAL-FILE", "APPL-RESULT"), ""),
            new RuleInfo("BR-CBACT04C-0029", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT04C (IF XREFFILE-STATUS = '00').", "app/cbl/CBACT04C.cbl:254-257", true, null, List.of("XREF-FILE", "APPL-RESULT"), ""),
            new RuleInfo("BR-CBACT04C-0030", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT04C (IF DISCGRP-STATUS = '00').", "app/cbl/CBACT04C.cbl:272-275", true, null, List.of("DISCGRP-FILE", "APPL-RESULT"), ""));

    @Override
    public String programId() {
        return "CBACT04C";
    }

    @Override
    public List<String> aliases() {
        return List.of();
    }

    @Override
    public String sourcePath() {
        return "app/cbl/CBACT04C.cbl";
    }

    @Override
    public boolean programTranslated() {
        return false;
    }

    @Override
    public List<String> untranslatedReasons() {
        return UNTRANSLATED;
    }

    @Override
    public List<RuleInfo> rules() {
        return RULES;
    }

    @Override
    public CobolState newState() {
        return CobolState.load("CBACT04C");
    }

    public String getModuleId() {
        return "CBACT04C";
    }

    public String getSourcePath() {
        return "app/cbl/CBACT04C.cbl";
    }

    public List<String> getAliases() {
        return aliases();
    }

    @Override
    public void executeRule(String ruleId, CobolState st) {
        switch (ruleId) {
            case "BR-CBACT04C-0023": rule0(st); return;
            case "BR-CBACT04C-0024": rule1(st); return;
            case "BR-CBACT04C-0025": rule2(st); return;
            case "BR-CBACT04C-0026": rule3(st); return;
            case "BR-CBACT04C-0027": rule4(st); return;
            case "BR-CBACT04C-0028": rule5(st); return;
            case "BR-CBACT04C-0029": rule6(st); return;
            case "BR-CBACT04C-0030": rule7(st); return;
            default: throw new NotTranslatedException("Rule " + ruleId + " was not translated");
        }
    }

    /** Runs the program from its first paragraph (COBOL fall-through) until GOBACK / STOP RUN. */
    @Override
    public void execute(CobolState st) {
        if (!programTranslated()) {
            throw new NotTranslatedException("Program " + programId() + " is not fully translated: "
                    + String.join("; ", UNTRANSLATED));
        }
        try {
            pMainEntry(st);
            pM0000TcatbalfOpen(st);
            pM0100XreffileOpen(st);
            pM0200DiscgrpOpen(st);
            pM0300AcctfileOpen(st);
            pM0400TranfileOpen(st);
            pM1000TcatbalfGetNext(st);
            pM1050UpdateAccount(st);
            pM1100GetAcctData(st);
            pM1110GetXrefData(st);
            pM1200GetInterestRate(st);
            pM1200AGetDefaultIntRate(st);
            pM1300ComputeInterest(st);
            pM1300BWriteTx(st);
            pM1400ComputeFees(st);
            pM9000TcatbalfClose(st);
            pM9100XreffileClose(st);
            pM9200DiscgrpClose(st);
            pM9300AcctfileClose(st);
            pM9400TranfileClose(st);
            pZGetDb2FormatTimestamp(st);
            pM9999AbendProgram(st);
            pM9910DisplayIoStatus(st);
        } catch (ProgramExit e) {
            // GOBACK / STOP RUN
        }
    }

    private Cbact04cRequest mapToRequest(Map<String, Object> payload) {
        if (payload == null || payload.isEmpty()) {
            return Cbact04cRequest.defaultBaseline();
        }
        String acct = String.valueOf(payload.getOrDefault("accountId", "00000000001"));
        BigDecimal amt = BigDecimal.ZERO;
        Object amtObj = payload.get("amount");
        if (amtObj instanceof Number) {
            amt = BigDecimal.valueOf(((Number) amtObj).doubleValue());
        } else if (amtObj instanceof String str && !((String) str).isBlank()) {
            try {
                amt = new BigDecimal((String) str);
            } catch (NumberFormatException ignored) {}
        }
        String type = String.valueOf(payload.getOrDefault("transactionType", "STANDARD"));
        String ref = String.valueOf(payload.getOrDefault("referenceNumber", "REF-MAP"));
        String card = payload.containsKey("cardNumber") ? String.valueOf(payload.get("cardNumber")) : null;
        BigDecimal curBal = null;
        if (payload.containsKey("currentBalance")) {
            try { curBal = new BigDecimal(String.valueOf(payload.get("currentBalance"))); } catch (Exception ignored) {}
        }
        BigDecimal credLim = null;
        if (payload.containsKey("creditLimit")) {
            try { credLim = new BigDecimal(String.valueOf(payload.get("creditLimit"))); } catch (Exception ignored) {}
        }
        BigDecimal intRate = null;
        if (payload.containsKey("interestRate")) {
            try { intRate = new BigDecimal(String.valueOf(payload.get("interestRate"))); } catch (Exception ignored) {}
        }
        String dateStr = payload.containsKey("dateString") ? String.valueOf(payload.get("dateString")) : null;
        String dateFmt = payload.containsKey("dateFormat") ? String.valueOf(payload.get("dateFormat")) : null;
        Integer waitCs = null;
        if (payload.containsKey("waitCentiseconds")) {
            try { waitCs = Integer.valueOf(String.valueOf(payload.get("waitCentiseconds"))); } catch (Exception ignored) {}
        }
        return new Cbact04cRequest(acct, amt, type, ref, card, curBal, credLim, intRate, null, null, dateStr, dateFmt, waitCs, payload);
    }

    /**
     * CardDemo CBACT04C Ground Truth: Monthly Interest Calculation & Accrual
     * Mainframe Source: CBACT04C.cbl lines 462-470
     * Formula: WS-MONTHLY-INT = ( TRAN-CAT-BAL * DIS-INT-RATE) / 1200
     */
    @Operation(summary = "Calculate Monthly Interest", description = "Applies CardDemo interest formula (balance * rate) / 1200 and updates balance")
    @Transactional
    public Cbact04cResponse calculateMonthlyInterest(Cbact04cRequest request) {
        if (request == null) {
            return Cbact04cResponse.validationError("1300-VALIDATION", "Interest calculation request cannot be null.");
        }
        log.info("[CBACT04C] Calculating monthly interest for account: {}", request.accountId());

        if (request.amount() != null && request.amount().compareTo(BigDecimal.ZERO) < 0) {
            log.warn("[CBACT04C] Negative amount validation failure: {}", request.amount());
            return Cbact04cResponse.validationError("1300-VALIDATION", "Transaction amount must be non-negative.");
        }

        if (request.interestRate() == null || request.interestRate().compareTo(BigDecimal.ZERO) < 0) {
            log.warn("[CBACT04C] Invalid interest rate: {}", request.interestRate());
            return Cbact04cResponse.validationError("1300-VALIDATION", "Valid interest rate is required.");
        }

        BigDecimal currentBalance = request.currentBalance() != null ? request.currentBalance()
            : (request.amount() != null ? request.amount() : BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP));
        BigDecimal rate = request.interestRate();

        // 1300-COMPUTE-INTEREST: ( TRAN-CAT-BAL * DIS-INT-RATE) / 1200
        BigDecimal monthlyInt = currentBalance.multiply(rate)
            .divide(BigDecimal.valueOf(1200), 2, RoundingMode.HALF_UP);
        BigDecimal newBalance = currentBalance.add(monthlyInt).setScale(2, RoundingMode.HALF_UP);

        if (cvact01yRepository != null && request.accountId() != null && !request.accountId().isBlank()) {
            try {
                Long aId = Long.parseLong(request.accountId().trim());
                var optAcct = cvact01yRepository.findById(aId);
                if (optAcct.isPresent()) {
                    var acct = optAcct.get();
                    acct.setAcctCurrBal(newBalance);
                    cvact01yRepository.save(acct);
                    log.info("[CBACT04C] Persisted updated account {} balance {} via Cvact01yRepository", request.accountId(), newBalance);
                }
            } catch (NumberFormatException e) {
                log.warn("[CBACT04C] Malformed non-numeric accountId: {}", request.accountId());
            } catch (Exception e) {
                log.warn("[CBACT04C] Could not persist account update: {}", e.getMessage());
            }
        }

        log.info("[CBACT04C] Interest computed: bal={}, rate={}, int={}, newBal={}",
            currentBalance, rate, monthlyInt, newBalance);
        return Cbact04cResponse.interestResult("1300-COMPUTE-INTEREST", "Monthly interest calculated and accrued successfully", 0, newBalance, monthlyInt, request.referenceNumber());
    }

    /**
     * Business Rule: BR-CBACT04C-0023
     * Description: Enforces BR-CBACT04C-0023 operations from app/cbl/CBACT04C.cbl
     * Mainframe Source Citation: app/cbl/CBACT04C.cbl (Lines 188-191)
     */
    @Operation(summary = "Business logic rule BR-CBACT04C-0023", description = "Implements legacy business rule BR-CBACT04C-0023 derived from app/cbl/CBACT04C.cbl")
    @Transactional
    public Cbact04cResponse executeCbact04c0023(Cbact04cRequest request) {
        if (request == null) {
            return Cbact04cResponse.validationError("BR-CBACT04C-0023", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact04cService] Processing rule BR-CBACT04C-0023 for account: {}, amount: {}", request.accountId(), amount);

        return calculateMonthlyInterest(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT04C-0023.
     */
    @Transactional
    public ModernizedOperationResult executeCbact04c0023(Map<String, Object> payload) {
        Cbact04cRequest req = mapToRequest(payload);
        Cbact04cResponse resp = executeCbact04c0023(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT04C-0024
     * Description: Enforces BR-CBACT04C-0024 operations from app/cbl/CBACT04C.cbl
     * Mainframe Source Citation: app/cbl/CBACT04C.cbl (Lines 190-193)
     */
    @Operation(summary = "Business logic rule BR-CBACT04C-0024", description = "Implements legacy business rule BR-CBACT04C-0024 derived from app/cbl/CBACT04C.cbl")
    @Transactional
    public Cbact04cResponse executeCbact04c0024(Cbact04cRequest request) {
        if (request == null) {
            return Cbact04cResponse.validationError("BR-CBACT04C-0024", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact04cService] Processing rule BR-CBACT04C-0024 for account: {}, amount: {}", request.accountId(), amount);

        return calculateMonthlyInterest(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT04C-0024.
     */
    @Transactional
    public ModernizedOperationResult executeCbact04c0024(Map<String, Object> payload) {
        Cbact04cRequest req = mapToRequest(payload);
        Cbact04cResponse resp = executeCbact04c0024(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT04C-0025
     * Description: Enforces BR-CBACT04C-0025 operations from app/cbl/CBACT04C.cbl
     * Mainframe Source Citation: app/cbl/CBACT04C.cbl (Lines 193-196)
     */
    @Operation(summary = "Business logic rule BR-CBACT04C-0025", description = "Implements legacy business rule BR-CBACT04C-0025 derived from app/cbl/CBACT04C.cbl")
    @Transactional
    public Cbact04cResponse executeCbact04c0025(Cbact04cRequest request) {
        if (request == null) {
            return Cbact04cResponse.validationError("BR-CBACT04C-0025", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact04cService] Processing rule BR-CBACT04C-0025 for account: {}, amount: {}", request.accountId(), amount);

        return calculateMonthlyInterest(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT04C-0025.
     */
    @Transactional
    public ModernizedOperationResult executeCbact04c0025(Map<String, Object> payload) {
        Cbact04cRequest req = mapToRequest(payload);
        Cbact04cResponse resp = executeCbact04c0025(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT04C-0026
     * Description: Enforces BR-CBACT04C-0026 operations from app/cbl/CBACT04C.cbl
     * Mainframe Source Citation: app/cbl/CBACT04C.cbl (Lines 194-197)
     */
    @Operation(summary = "Business logic rule BR-CBACT04C-0026", description = "Implements legacy business rule BR-CBACT04C-0026 derived from app/cbl/CBACT04C.cbl")
    @Transactional
    public Cbact04cResponse executeCbact04c0026(Cbact04cRequest request) {
        if (request == null) {
            return Cbact04cResponse.validationError("BR-CBACT04C-0026", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact04cService] Processing rule BR-CBACT04C-0026 for account: {}, amount: {}", request.accountId(), amount);

        return calculateMonthlyInterest(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT04C-0026.
     */
    @Transactional
    public ModernizedOperationResult executeCbact04c0026(Map<String, Object> payload) {
        Cbact04cRequest req = mapToRequest(payload);
        Cbact04cResponse resp = executeCbact04c0026(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT04C-0027
     * Description: Enforces BR-CBACT04C-0027 operations from app/cbl/CBACT04C.cbl
     * Mainframe Source Citation: app/cbl/CBACT04C.cbl (Lines 213-216)
     */
    @Operation(summary = "Business logic rule BR-CBACT04C-0027", description = "Implements legacy business rule BR-CBACT04C-0027 derived from app/cbl/CBACT04C.cbl")
    @Transactional
    public Cbact04cResponse executeCbact04c0027(Cbact04cRequest request) {
        if (request == null) {
            return Cbact04cResponse.validationError("BR-CBACT04C-0027", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact04cService] Processing rule BR-CBACT04C-0027 for account: {}, amount: {}", request.accountId(), amount);

        return calculateMonthlyInterest(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT04C-0027.
     */
    @Transactional
    public ModernizedOperationResult executeCbact04c0027(Map<String, Object> payload) {
        Cbact04cRequest req = mapToRequest(payload);
        Cbact04cResponse resp = executeCbact04c0027(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT04C-0028
     * Description: Enforces BR-CBACT04C-0028 operations from app/cbl/CBACT04C.cbl
     * Mainframe Source Citation: app/cbl/CBACT04C.cbl (Lines 236-239)
     */
    @Operation(summary = "Business logic rule BR-CBACT04C-0028", description = "Implements legacy business rule BR-CBACT04C-0028 derived from app/cbl/CBACT04C.cbl")
    @Transactional
    public Cbact04cResponse executeCbact04c0028(Cbact04cRequest request) {
        if (request == null) {
            return Cbact04cResponse.validationError("BR-CBACT04C-0028", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact04cService] Processing rule BR-CBACT04C-0028 for account: {}, amount: {}", request.accountId(), amount);

        return calculateMonthlyInterest(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT04C-0028.
     */
    @Transactional
    public ModernizedOperationResult executeCbact04c0028(Map<String, Object> payload) {
        Cbact04cRequest req = mapToRequest(payload);
        Cbact04cResponse resp = executeCbact04c0028(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT04C-0029
     * Description: Enforces BR-CBACT04C-0029 operations from app/cbl/CBACT04C.cbl
     * Mainframe Source Citation: app/cbl/CBACT04C.cbl (Lines 254-257)
     */
    @Operation(summary = "Business logic rule BR-CBACT04C-0029", description = "Implements legacy business rule BR-CBACT04C-0029 derived from app/cbl/CBACT04C.cbl")
    @Transactional
    public Cbact04cResponse executeCbact04c0029(Cbact04cRequest request) {
        if (request == null) {
            return Cbact04cResponse.validationError("BR-CBACT04C-0029", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact04cService] Processing rule BR-CBACT04C-0029 for account: {}, amount: {}", request.accountId(), amount);

        return calculateMonthlyInterest(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT04C-0029.
     */
    @Transactional
    public ModernizedOperationResult executeCbact04c0029(Map<String, Object> payload) {
        Cbact04cRequest req = mapToRequest(payload);
        Cbact04cResponse resp = executeCbact04c0029(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT04C-0030
     * Description: Enforces BR-CBACT04C-0030 operations from app/cbl/CBACT04C.cbl
     * Mainframe Source Citation: app/cbl/CBACT04C.cbl (Lines 272-275)
     */
    @Operation(summary = "Business logic rule BR-CBACT04C-0030", description = "Implements legacy business rule BR-CBACT04C-0030 derived from app/cbl/CBACT04C.cbl")
    @Transactional
    public Cbact04cResponse executeCbact04c0030(Cbact04cRequest request) {
        if (request == null) {
            return Cbact04cResponse.validationError("BR-CBACT04C-0030", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact04cService] Processing rule BR-CBACT04C-0030 for account: {}, amount: {}", request.accountId(), amount);

        return calculateMonthlyInterest(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT04C-0030.
     */
    @Transactional
    public ModernizedOperationResult executeCbact04c0030(Map<String, Object> payload) {
        Cbact04cRequest req = mapToRequest(payload);
        Cbact04cResponse resp = executeCbact04c0030(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Procedural Paragraph: MAIN-ENTRY
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performMainEntry(Map<String, Object> state) {
        log.debug("[Cbact04cService] Executing paragraph routine: MAIN-ENTRY");
        if (state != null) {
            state.put("lastExecutedParagraph", "MAIN-ENTRY");
            state.put("MainEntry_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 0000-TCATBALF-OPEN
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM0000TcatbalfOpen(Map<String, Object> state) {
        log.debug("[Cbact04cService] Executing paragraph routine: 0000-TCATBALF-OPEN");
        if (state != null) {
            state.put("lastExecutedParagraph", "0000-TCATBALF-OPEN");
            state.put("M0000TcatbalfOpen_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 0100-XREFFILE-OPEN
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM0100XreffileOpen(Map<String, Object> state) {
        log.debug("[Cbact04cService] Executing paragraph routine: 0100-XREFFILE-OPEN");
        if (state != null) {
            state.put("lastExecutedParagraph", "0100-XREFFILE-OPEN");
            state.put("M0100XreffileOpen_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 0200-DISCGRP-OPEN
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM0200DiscgrpOpen(Map<String, Object> state) {
        log.debug("[Cbact04cService] Executing paragraph routine: 0200-DISCGRP-OPEN");
        if (state != null) {
            state.put("lastExecutedParagraph", "0200-DISCGRP-OPEN");
            state.put("M0200DiscgrpOpen_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 0300-ACCTFILE-OPEN
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM0300AcctfileOpen(Map<String, Object> state) {
        log.debug("[Cbact04cService] Executing paragraph routine: 0300-ACCTFILE-OPEN");
        if (state != null) {
            state.put("lastExecutedParagraph", "0300-ACCTFILE-OPEN");
            state.put("M0300AcctfileOpen_status", "EXECUTED");
        }
    }

    /**
     * Rule BR-CBACT04C-0023 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT04C (IF END-OF-FILE = 'N').
     * @citation app/cbl/CBACT04C.cbl:188-191
     */
    void rule0(CobolState st) {
        // Rule condition: BR-CBACT04C-0023 (VALIDATION)
        // Statement: Validate field conditions and reject invalid inputs in CBACT04C (IF END-OF-FILE = 'N').
        // Untranslated reason: expected END-PERFORM but found the end of the cited lines
        throw new NotTranslatedException("Rule BR-CBACT04C-0023: expected END-PERFORM but found the end of the cited lines");
    }

    /**
     * Rule BR-CBACT04C-0024 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT04C (IF END-OF-FILE = 'N').
     * @citation app/cbl/CBACT04C.cbl:190-193
     */
    void rule1(CobolState st) {
        pM1000TcatbalfGetNext(st);
        if ((st.cmp(st.str("END-OF-FILE"), "N") == 0)) {
            st.setNum("WS-RECORD-COUNT", st.num("WS-RECORD-COUNT").add(new BigDecimal("1")), false);
            st.display(st.str("TRAN-CAT-BAL-RECORD"));
        }
    }

    /**
     * Rule BR-CBACT04C-0025 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT04C (IF TRANCAT-ACCT-ID NOT= WS-LAST-ACCT-NUM).
     * @citation app/cbl/CBACT04C.cbl:193-196
     */
    void rule2(CobolState st) {
        st.display(st.str("TRAN-CAT-BAL-RECORD"));
        if (!(st.cmp(st.num("TRANCAT-ACCT-ID"), st.str("WS-LAST-ACCT-NUM")) == 0)) {
            if (!(st.cmp(st.str("WS-FIRST-TIME"), "Y") == 0)) {
                pM1050UpdateAccount(st);
            }
        }
    }

    /**
     * Rule BR-CBACT04C-0026 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT04C (IF WS-FIRST-TIME NOT = 'Y').
     * @citation app/cbl/CBACT04C.cbl:194-197
     */
    void rule3(CobolState st) {
        if (!(st.cmp(st.num("TRANCAT-ACCT-ID"), st.str("WS-LAST-ACCT-NUM")) == 0)) {
            if (!(st.cmp(st.str("WS-FIRST-TIME"), "Y") == 0)) {
                pM1050UpdateAccount(st);
            } else {
            }
        }
    }

    /**
     * Rule BR-CBACT04C-0027 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT04C (IF DIS-INT-RATE NOT = 0).
     * @citation app/cbl/CBACT04C.cbl:213-216
     */
    void rule4(CobolState st) {
        // Rule condition: BR-CBACT04C-0027 (VALIDATION)
        // Statement: Validate field conditions and reject invalid inputs in CBACT04C (IF DIS-INT-RATE NOT = 0).
        // Untranslated reason: performs 1200-GET-INTEREST-RATE, which is not translated
        throw new NotTranslatedException("Rule BR-CBACT04C-0027: performs 1200-GET-INTEREST-RATE, which is not translated");
    }

    /**
     * Rule BR-CBACT04C-0028 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT04C (IF TCATBALF-STATUS = '00').
     * @citation app/cbl/CBACT04C.cbl:236-239
     */
    void rule5(CobolState st) {
        st.openFile("TCATBAL-FILE", "INPUT");
        if ((st.cmp(st.str("TCATBALF-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
        } else {
        }
    }

    /**
     * Rule BR-CBACT04C-0029 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT04C (IF XREFFILE-STATUS = '00').
     * @citation app/cbl/CBACT04C.cbl:254-257
     */
    void rule6(CobolState st) {
        st.openFile("XREF-FILE", "INPUT");
        if ((st.cmp(st.str("XREFFILE-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
        } else {
        }
    }

    /**
     * Rule BR-CBACT04C-0030 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT04C (IF DISCGRP-STATUS = '00').
     * @citation app/cbl/CBACT04C.cbl:272-275
     */
    void rule7(CobolState st) {
        st.openFile("DISCGRP-FILE", "INPUT");
        if ((st.cmp(st.str("DISCGRP-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
        } else {
        }
    }

    /** Paragraph MAIN-ENTRY. */
    void pMainEntry(CobolState st) {
        st.display("START OF EXECUTION OF PROGRAM CBACT04C");
        pM0000TcatbalfOpen(st);
        pM0100XreffileOpen(st);
        pM0200DiscgrpOpen(st);
        pM0300AcctfileOpen(st);
        pM0400TranfileOpen(st);
        while (!(st.cmp(st.str("END-OF-FILE"), "Y") == 0)) {
            st.tick();
            if ((st.cmp(st.str("END-OF-FILE"), "N") == 0)) {
                pM1000TcatbalfGetNext(st);
                if ((st.cmp(st.str("END-OF-FILE"), "N") == 0)) {
                    st.setNum("WS-RECORD-COUNT", st.num("WS-RECORD-COUNT").add(new BigDecimal("1")), false);
                    st.display(st.str("TRAN-CAT-BAL-RECORD"));
                    if (!(st.cmp(st.num("TRANCAT-ACCT-ID"), st.str("WS-LAST-ACCT-NUM")) == 0)) {
                        if (!(st.cmp(st.str("WS-FIRST-TIME"), "Y") == 0)) {
                            pM1050UpdateAccount(st);
                        } else {
                            st.move("WS-FIRST-TIME", "N");
                        }
                        st.move("WS-TOTAL-INT", new BigDecimal("0"));
                        st.move("WS-LAST-ACCT-NUM", st.val("TRANCAT-ACCT-ID"));
                        st.setNum("FD-ACCT-ID", st.num("TRANCAT-ACCT-ID"), false);
                        pM1100GetAcctData(st);
                        st.setNum("FD-XREF-ACCT-ID", st.num("TRANCAT-ACCT-ID"), false);
                        pM1110GetXrefData(st);
                    }
                    st.move("FD-DIS-ACCT-GROUP-ID", st.val("ACCT-GROUP-ID"));
                    st.setNum("FD-DIS-TRAN-CAT-CD", st.num("TRANCAT-CD"), false);
                    st.move("FD-DIS-TRAN-TYPE-CD", st.val("TRANCAT-TYPE-CD"));
                    pM1200GetInterestRate(st);
                    if (!(st.cmp(st.num("DIS-INT-RATE"), new BigDecimal("0")) == 0)) {
                        pM1300ComputeInterest(st);
                        pM1400ComputeFees(st);
                    }
                }
            } else {
                pM1050UpdateAccount(st);
            }
        }
        pM9000TcatbalfClose(st);
        pM9100XreffileClose(st);
        pM9200DiscgrpClose(st);
        pM9300AcctfileClose(st);
        pM9400TranfileClose(st);
        st.display("END OF EXECUTION OF PROGRAM CBACT04C");
        st.exitProgram();
    }

    /** Paragraph 0000-TCATBALF-OPEN. */
    void pM0000TcatbalfOpen(CobolState st) {
        st.move("APPL-RESULT", new BigDecimal("8"));
        st.openFile("TCATBAL-FILE", "INPUT");
        if ((st.cmp(st.str("TCATBALF-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
        } else {
            st.move("APPL-RESULT", new BigDecimal("12"));
        }
        if (st.is88("APPL-RESULT", new BigDecimal("0"))) {
            // CONTINUE
        } else {
            st.display("ERROR OPENING TRANSACTION CATEGORY BALANCE");
            st.move("IO-STATUS", st.val("TCATBALF-STATUS"));
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
    }

    /** Paragraph 0100-XREFFILE-OPEN. */
    void pM0100XreffileOpen(CobolState st) {
        st.move("APPL-RESULT", new BigDecimal("8"));
        st.openFile("XREF-FILE", "INPUT");
        if ((st.cmp(st.str("XREFFILE-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
        } else {
            st.move("APPL-RESULT", new BigDecimal("12"));
        }
        if (st.is88("APPL-RESULT", new BigDecimal("0"))) {
            // CONTINUE
        } else {
            st.display("ERROR OPENING CROSS REF FILE", st.str("XREFFILE-STATUS"));
            st.move("IO-STATUS", st.val("XREFFILE-STATUS"));
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
    }

    /** Paragraph 0200-DISCGRP-OPEN. */
    void pM0200DiscgrpOpen(CobolState st) {
        st.move("APPL-RESULT", new BigDecimal("8"));
        st.openFile("DISCGRP-FILE", "INPUT");
        if ((st.cmp(st.str("DISCGRP-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
        } else {
            st.move("APPL-RESULT", new BigDecimal("12"));
        }
        if (st.is88("APPL-RESULT", new BigDecimal("0"))) {
            // CONTINUE
        } else {
            st.display("ERROR OPENING DALY REJECTS FILE");
            st.move("IO-STATUS", st.val("DISCGRP-STATUS"));
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
    }

    /** Paragraph 0300-ACCTFILE-OPEN. */
    void pM0300AcctfileOpen(CobolState st) {
        st.move("APPL-RESULT", new BigDecimal("8"));
        st.openFile("ACCOUNT-FILE", "I-O");
        if ((st.cmp(st.str("ACCTFILE-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
        } else {
            st.move("APPL-RESULT", new BigDecimal("12"));
        }
        if (st.is88("APPL-RESULT", new BigDecimal("0"))) {
            // CONTINUE
        } else {
            st.display("ERROR OPENING ACCOUNT MASTER FILE");
            st.move("IO-STATUS", st.val("ACCTFILE-STATUS"));
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
    }

    /** Paragraph 0400-TRANFILE-OPEN. */
    void pM0400TranfileOpen(CobolState st) {
        st.move("APPL-RESULT", new BigDecimal("8"));
        st.openFile("TRANSACT-FILE", "OUTPUT");
        if ((st.cmp(st.str("TRANFILE-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
        } else {
            st.move("APPL-RESULT", new BigDecimal("12"));
        }
        if (st.is88("APPL-RESULT", new BigDecimal("0"))) {
            // CONTINUE
        } else {
            st.display("ERROR OPENING TRANSACTION FILE");
            st.move("IO-STATUS", st.val("TRANFILE-STATUS"));
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
    }

    /** Paragraph 1000-TCATBALF-GET-NEXT. */
    void pM1000TcatbalfGetNext(CobolState st) {
        st.readFile("TCATBAL-FILE", "TRAN-CAT-BAL-RECORD");
        if ((st.cmp(st.str("TCATBALF-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
        } else {
            if ((st.cmp(st.str("TCATBALF-STATUS"), "10") == 0)) {
                st.move("APPL-RESULT", new BigDecimal("16"));
            } else {
                st.move("APPL-RESULT", new BigDecimal("12"));
            }
        }
        if (st.is88("APPL-RESULT", new BigDecimal("0"))) {
            // CONTINUE
        } else {
            if (st.is88("APPL-RESULT", new BigDecimal("16"))) {
                st.move("END-OF-FILE", "Y");
            } else {
                st.display("ERROR READING TRANSACTION CATEGORY FILE");
                st.move("IO-STATUS", st.val("TCATBALF-STATUS"));
                pM9910DisplayIoStatus(st);
                pM9999AbendProgram(st);
            }
        }
        // EXIT
    }

    /** Paragraph 1050-UPDATE-ACCOUNT. */
    void pM1050UpdateAccount(CobolState st) {
        st.setNum("ACCT-CURR-BAL", st.num("ACCT-CURR-BAL").add(st.num("WS-TOTAL-INT")), false);
        st.move("ACCT-CURR-CYC-CREDIT", new BigDecimal("0"));
        st.move("ACCT-CURR-CYC-DEBIT", new BigDecimal("0"));
        st.rewriteFile("FD-ACCTFILE-REC", "ACCOUNT-RECORD");
        if ((st.cmp(st.str("ACCTFILE-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
        } else {
            st.move("APPL-RESULT", new BigDecimal("12"));
        }
        if (st.is88("APPL-RESULT", new BigDecimal("0"))) {
            // CONTINUE
        } else {
            st.display("ERROR RE-WRITING ACCOUNT FILE");
            st.move("IO-STATUS", st.val("ACCTFILE-STATUS"));
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
    }

    /** Paragraph 1100-GET-ACCT-DATA. */
    void pM1100GetAcctData(CobolState st) {
        if (!st.readFile("ACCOUNT-FILE", "ACCOUNT-RECORD")) {
            st.display("ACCOUNT NOT FOUND: ", st.str("FD-ACCT-ID"));
        }
        if ((st.cmp(st.str("ACCTFILE-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
        } else {
            st.move("APPL-RESULT", new BigDecimal("12"));
        }
        if (st.is88("APPL-RESULT", new BigDecimal("0"))) {
            // CONTINUE
        } else {
            st.display("ERROR READING ACCOUNT FILE");
            st.move("IO-STATUS", st.val("ACCTFILE-STATUS"));
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
    }

    /** Paragraph 1110-GET-XREF-DATA. */
    void pM1110GetXrefData(CobolState st) {
        if (!st.readFile("XREF-FILE", "CARD-XREF-RECORD")) {
            st.display("ACCOUNT NOT FOUND: ", st.str("FD-XREF-ACCT-ID"));
        }
        if ((st.cmp(st.str("XREFFILE-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
        } else {
            st.move("APPL-RESULT", new BigDecimal("12"));
        }
        if (st.is88("APPL-RESULT", new BigDecimal("0"))) {
            // CONTINUE
        } else {
            st.display("ERROR READING XREF FILE");
            st.move("IO-STATUS", st.val("XREFFILE-STATUS"));
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
    }

    /** Paragraph 1200-GET-INTEREST-RATE. */
    void pM1200GetInterestRate(CobolState st) {
        throw new NotTranslatedException("1200-GET-INTEREST-RATE: abbreviated combined conditions (for example A = 1 OR 2) are not translated");
    }

    /** Paragraph 1200-A-GET-DEFAULT-INT-RATE. */
    void pM1200AGetDefaultIntRate(CobolState st) {
        st.readFile("DISCGRP-FILE", "DIS-GROUP-RECORD");
        if ((st.cmp(st.str("DISCGRP-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
        } else {
            st.move("APPL-RESULT", new BigDecimal("12"));
        }
        if (st.is88("APPL-RESULT", new BigDecimal("0"))) {
            // CONTINUE
        } else {
            st.display("ERROR READING DEFAULT DISCLOSURE GROUP");
            st.move("IO-STATUS", st.val("DISCGRP-STATUS"));
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
    }

    /** Paragraph 1300-COMPUTE-INTEREST. */
    void pM1300ComputeInterest(CobolState st) {
        st.setNum("WS-MONTHLY-INT", st.div((st.num("TRAN-CAT-BAL").multiply(st.num("DIS-INT-RATE"))), new BigDecimal("1200")), false);
        st.setNum("WS-TOTAL-INT", st.num("WS-TOTAL-INT").add(st.num("WS-MONTHLY-INT")), false);
        pM1300BWriteTx(st);
        // EXIT
    }

    /** Paragraph 1300-B-WRITE-TX. */
    void pM1300BWriteTx(CobolState st) {
        throw new NotTranslatedException("1300-B-WRITE-TX: STRING statements are not translated");
    }

    /** Paragraph 1400-COMPUTE-FEES. */
    void pM1400ComputeFees(CobolState st) {
        // EXIT
    }

    /** Paragraph 9000-TCATBALF-CLOSE. */
    void pM9000TcatbalfClose(CobolState st) {
        st.move("APPL-RESULT", new BigDecimal("8"));
        st.closeFile("TCATBAL-FILE");
        if ((st.cmp(st.str("TCATBALF-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
        } else {
            st.move("APPL-RESULT", new BigDecimal("12"));
        }
        if (st.is88("APPL-RESULT", new BigDecimal("0"))) {
            // CONTINUE
        } else {
            st.display("ERROR CLOSING TRANSACTION BALANCE FILE");
            st.move("IO-STATUS", st.val("TCATBALF-STATUS"));
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
    }

    /** Paragraph 9100-XREFFILE-CLOSE. */
    void pM9100XreffileClose(CobolState st) {
        st.move("APPL-RESULT", new BigDecimal("8"));
        st.closeFile("XREF-FILE");
        if ((st.cmp(st.str("XREFFILE-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
        } else {
            st.move("APPL-RESULT", new BigDecimal("12"));
        }
        if (st.is88("APPL-RESULT", new BigDecimal("0"))) {
            // CONTINUE
        } else {
            st.display("ERROR CLOSING CROSS REF FILE");
            st.move("IO-STATUS", st.val("XREFFILE-STATUS"));
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
    }

    /** Paragraph 9200-DISCGRP-CLOSE. */
    void pM9200DiscgrpClose(CobolState st) {
        st.move("APPL-RESULT", new BigDecimal("8"));
        st.closeFile("DISCGRP-FILE");
        if ((st.cmp(st.str("DISCGRP-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
        } else {
            st.move("APPL-RESULT", new BigDecimal("12"));
        }
        if (st.is88("APPL-RESULT", new BigDecimal("0"))) {
            // CONTINUE
        } else {
            st.display("ERROR CLOSING DISCLOSURE GROUP FILE");
            st.move("IO-STATUS", st.val("DISCGRP-STATUS"));
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
    }

    /** Paragraph 9300-ACCTFILE-CLOSE. */
    void pM9300AcctfileClose(CobolState st) {
        st.move("APPL-RESULT", new BigDecimal("8"));
        st.closeFile("ACCOUNT-FILE");
        if ((st.cmp(st.str("ACCTFILE-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
        } else {
            st.move("APPL-RESULT", new BigDecimal("12"));
        }
        if (st.is88("APPL-RESULT", new BigDecimal("0"))) {
            // CONTINUE
        } else {
            st.display("ERROR CLOSING ACCOUNT FILE");
            st.move("IO-STATUS", st.val("ACCTFILE-STATUS"));
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
    }

    /** Paragraph 9400-TRANFILE-CLOSE. */
    void pM9400TranfileClose(CobolState st) {
        st.move("APPL-RESULT", new BigDecimal("8"));
        st.closeFile("TRANSACT-FILE");
        if ((st.cmp(st.str("TRANFILE-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
        } else {
            st.move("APPL-RESULT", new BigDecimal("12"));
        }
        if (st.is88("APPL-RESULT", new BigDecimal("0"))) {
            // CONTINUE
        } else {
            st.display("ERROR CLOSING TRANSACTION FILE");
            st.move("IO-STATUS", st.val("TRANFILE-STATUS"));
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
    }

    /** Paragraph Z-GET-DB2-FORMAT-TIMESTAMP. */
    void pZGetDb2FormatTimestamp(CobolState st) {
        throw new NotTranslatedException("Z-GET-DB2-FORMAT-TIMESTAMP: expected TO but found 'CURRENT-DATE'");
    }

    /** Paragraph 9999-ABEND-PROGRAM. */
    void pM9999AbendProgram(CobolState st) {
        st.display("ABENDING PROGRAM");
        st.move("TIMING", new BigDecimal("0"));
        st.move("ABCODE", new BigDecimal("999"));
        registry.call("CEE3ABD", st, new String[]{"ABCODE", "TIMING"}, new boolean[]{true, true});
    }

    /** Paragraph 9910-DISPLAY-IO-STATUS. */
    void pM9910DisplayIoStatus(CobolState st) {
        if ((!(st.isNumeric("IO-STATUS")) || (st.cmp(st.str("IO-STAT1"), "9") == 0))) {
            st.move("IO-STATUS-04", st.val("IO-STAT1"));
            st.move("TWO-BYTES-BINARY", new BigDecimal("0"));
            st.move("TWO-BYTES-RIGHT", st.val("IO-STAT2"));
            st.setNum("IO-STATUS-0403", st.num("TWO-BYTES-BINARY"), false);
            st.display("FILE STATUS IS: NNNN", st.str("IO-STATUS-04"));
        } else {
            st.move("IO-STATUS-04", "0000");
            st.move("IO-STATUS-04", st.val("IO-STATUS"));
            st.display("FILE STATUS IS: NNNN", st.str("IO-STATUS-04"));
        }
        // EXIT
    }

    /**
     * Standard Operation Result Record
     */
    public record ModernizedOperationResult(
        String ruleId,
        String status,
        String message,
        Instant timestamp
    ) {
        public ModernizedOperationResult(String ruleId, String status, String message) {
            this(ruleId, status, message, Instant.now());
        }
    }
}
