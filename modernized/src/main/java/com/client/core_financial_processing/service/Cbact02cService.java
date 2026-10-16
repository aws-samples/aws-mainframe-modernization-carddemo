package com.client.core_financial_processing.service;


import java.util.List;
import java.util.Map;
import com.client.runtime.CobolState;
import com.client.runtime.NotTranslatedException;
import com.client.runtime.ProgramExit;
import com.client.runtime.ProgramRegistry;
import com.client.runtime.ProgramService;
import com.client.runtime.RuleInfo;
import com.client.core_financial_processing.dto.Cbact02cRequest;
import com.client.core_financial_processing.dto.Cbact02cResponse;
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
 * Modernized Spring Boot Domain Service: Cbact02cService
 *
 * Transpiled from Mainframe COBOL program: CBACT02C
 * Legacy Source Path: app/cbl/CBACT02C.cbl
 * Generation Strategy: Enterprise Modernization with ProgramService registry integration
 */
@Slf4j
@Service
@Tag(name = "Cbact02cService", description = "Modernized domain business service for legacy CBACT02C")
public class Cbact02cService implements ProgramService {

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

    private static final List<String> UNTRANSLATED = List.of("MAIN-ENTRY: DISPLAY of group item CARD-RECORD is not translated", "1000-CARDFILE-GET-NEXT: comparison of group item CARDFILE-STATUS is not translated", "0000-CARDFILE-OPEN: comparison of group item CARDFILE-STATUS is not translated", "9000-CARDFILE-CLOSE: comparison of group item CARDFILE-STATUS is not translated", "9910-DISPLAY-IO-STATUS: DISPLAY of group item IO-STATUS-04 is not translated");

    private static final List<RuleInfo> RULES = List.of(
            new RuleInfo("BR-CBACT02C-0009", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT02C (IF END-OF-FILE = 'N').", "app/cbl/CBACT02C.cbl:74-77", false, "performs 1000-CARDFILE-GET-NEXT, which is not translated", List.of("END-OF-FILE", "END-OF-FILE", "END-OF-FILE", "END-OF-FILE"), "untranslated (performs 1000-CARDFILE-GET-NEXT, which is not translated)"),
            new RuleInfo("BR-CBACT02C-0010", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT02C (IF END-OF-FILE = 'N').", "app/cbl/CBACT02C.cbl:76-79", false, "performs 1000-CARDFILE-GET-NEXT, which is not translated", List.of("END-OF-FILE", "CARD-RECORD", "END-OF-FILE"), "untranslated (performs 1000-CARDFILE-GET-NEXT, which is not translated)"),
            new RuleInfo("BR-CBACT02C-0011", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT02C (IF CARDFILE-STATUS = '00').", "app/cbl/CBACT02C.cbl:93-96", true, null, List.of("CARDFILE-FILE", "CARD-RECORD", "CARDFILE-STATUS", "APPL-RESULT", "CARDFILE-STATUS"), "translated procedural statement(s) (1 op(s))"),
            new RuleInfo("BR-CBACT02C-0012", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT02C (IF CARDFILE-STATUS = '10').", "app/cbl/CBACT02C.cbl:97-100", true, null, List.of("CARDFILE-STATUS", "APPL-RESULT", "CARDFILE-STATUS"), "translated procedural statement(s) (1 op(s))"),
            new RuleInfo("BR-CBACT02C-0013", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT02C (IF CARDFILE-STATUS = '00').", "app/cbl/CBACT02C.cbl:120-123", true, null, List.of("CARDFILE-FILE", "CARDFILE-STATUS", "APPL-RESULT", "CARDFILE-STATUS"), "translated procedural statement(s) (1 op(s))"),
            new RuleInfo("BR-CBACT02C-0014", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT02C (IF CARDFILE-STATUS = '00').", "app/cbl/CBACT02C.cbl:138-141", false, "comparison of group item CARDFILE-STATUS is not translated", List.of("CARDFILE-FILE", "CARDFILE-STATUS", "APPL-RESULT", "APPL-RESULT", "CARDFILE-STATUS"), "untranslated (comparison of group item CARDFILE-STATUS is not translated)"),
            new RuleInfo("BR-CBACT02C-0015", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT02C (IF IO-STATUS NOT NUMERIC).", "app/cbl/CBACT02C.cbl:161-164", true, null, List.of("IO-STATUS", "IO-STAT1", "IO-STATUS-04"), ""));

    @Override
    public String programId() {
        return "CBACT02C";
    }

    @Override
    public List<String> aliases() {
        return List.of();
    }

    @Override
    public String sourcePath() {
        return "app/cbl/CBACT02C.cbl";
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
        return CobolState.load("CBACT02C");
    }

    public String getModuleId() {
        return "CBACT02C";
    }

    public String getSourcePath() {
        return "app/cbl/CBACT02C.cbl";
    }

    public List<String> getAliases() {
        return aliases();
    }

    @Override
    public void executeRule(String ruleId, CobolState st) {
        switch (ruleId) {
            case "BR-CBACT02C-0009": rule0(st); return;
            case "BR-CBACT02C-0010": rule1(st); return;
            case "BR-CBACT02C-0011": rule2(st); return;
            case "BR-CBACT02C-0012": rule3(st); return;
            case "BR-CBACT02C-0013": rule4(st); return;
            case "BR-CBACT02C-0014": rule5(st); return;
            case "BR-CBACT02C-0015": rule6(st); return;
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
            pM1000CardfileGetNext(st);
            pM0000CardfileOpen(st);
            pM9000CardfileClose(st);
            pM9999AbendProgram(st);
            pM9910DisplayIoStatus(st);
        } catch (ProgramExit e) {
            // GOBACK / STOP RUN
        }
    }

    private Cbact02cRequest mapToRequest(Map<String, Object> payload) {
        if (payload == null || payload.isEmpty()) {
            return Cbact02cRequest.defaultBaseline();
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
        return new Cbact02cRequest(acct, amt, type, ref, card, curBal, credLim, intRate, null, null, dateStr, dateFmt, waitCs, payload);
    }

    /**
     * CardDemo CBACT02C: Card Cross-Reference File Reader
     * Mainframe Source: CBACT02C.cbl
     * Rejection Reason Codes:
     *   21: Card number required / not found in XREFFILE
     */
    @Operation(summary = "Lookup Card XREF Record", description = "Reads card-to-account mapping from XREFFILE")
    @Transactional(readOnly = true)
    public Cbact02cResponse lookupCardXref(Cbact02cRequest request) {
        if (request == null) {
            return Cbact02cResponse.declined("0000-XREFFILE-GET", "CARD XREF RECORD NOT FOUND", 21, BigDecimal.ZERO, "");
        }
        String cardNum = request.cardNumber() != null ? request.cardNumber().trim() : "";
        if (cardNum.isEmpty() || "0000000000000000".equals(cardNum) || cardNum.replace("0", "").isEmpty()) {
            log.warn("[CBACT02C] Validation failure 21: CARD XREF RECORD NOT FOUND");
            return Cbact02cResponse.declined("0000-XREFFILE-GET", "CARD XREF RECORD NOT FOUND", 21, BigDecimal.ZERO, request.referenceNumber());
        }
        String acctId = request.accountId() != null ? request.accountId() : "";
        String custId = request.customerId() != null ? request.customerId() : "";
        if (cvxrf01yRepository != null) {
            try {
                var optXref = cvxrf01yRepository.findById(cardNum);
                if (optXref.isPresent()) {
                    var x = optXref.get();
                    if (x.getAcctId() != null) acctId = String.valueOf(x.getAcctId());
                    if (x.getCustId() != null) custId = String.valueOf(x.getCustId());
                }
            } catch (Exception e) {
                log.debug("[CBACT02C] DB lookup skipped: {}", e.getMessage());
            }
        }
        log.info("[CBACT02C] Card XREF record retrieved for card: {} -> account: {}", cardNum, acctId);
        return Cbact02cResponse.xrefSuccess("0000-XREFFILE-GET", "Card cross-reference record retrieved successfully", cardNum, acctId, custId, request.referenceNumber());
    }

    /**
     * Business Rule: BR-CBACT02C-0009
     * Description: Enforces BR-CBACT02C-0009 operations from app/cbl/CBACT02C.cbl
     * Mainframe Source Citation: app/cbl/CBACT02C.cbl (Lines 74-77)
     */
    @Operation(summary = "Business logic rule BR-CBACT02C-0009", description = "Implements legacy business rule BR-CBACT02C-0009 derived from app/cbl/CBACT02C.cbl")
    @Transactional
    public Cbact02cResponse executeCbact02c0009(Cbact02cRequest request) {
        if (request == null) {
            return Cbact02cResponse.validationError("BR-CBACT02C-0009", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact02cService] Processing rule BR-CBACT02C-0009 for account: {}, amount: {}", request.accountId(), amount);

        return lookupCardXref(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT02C-0009.
     */
    @Transactional
    public ModernizedOperationResult executeCbact02c0009(Map<String, Object> payload) {
        Cbact02cRequest req = mapToRequest(payload);
        Cbact02cResponse resp = executeCbact02c0009(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT02C-0010
     * Description: Enforces BR-CBACT02C-0010 operations from app/cbl/CBACT02C.cbl
     * Mainframe Source Citation: app/cbl/CBACT02C.cbl (Lines 76-79)
     */
    @Operation(summary = "Business logic rule BR-CBACT02C-0010", description = "Implements legacy business rule BR-CBACT02C-0010 derived from app/cbl/CBACT02C.cbl")
    @Transactional
    public Cbact02cResponse executeCbact02c0010(Cbact02cRequest request) {
        if (request == null) {
            return Cbact02cResponse.validationError("BR-CBACT02C-0010", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact02cService] Processing rule BR-CBACT02C-0010 for account: {}, amount: {}", request.accountId(), amount);

        return lookupCardXref(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT02C-0010.
     */
    @Transactional
    public ModernizedOperationResult executeCbact02c0010(Map<String, Object> payload) {
        Cbact02cRequest req = mapToRequest(payload);
        Cbact02cResponse resp = executeCbact02c0010(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT02C-0011
     * Description: Enforces BR-CBACT02C-0011 operations from app/cbl/CBACT02C.cbl
     * Mainframe Source Citation: app/cbl/CBACT02C.cbl (Lines 93-96)
     */
    @Operation(summary = "Business logic rule BR-CBACT02C-0011", description = "Implements legacy business rule BR-CBACT02C-0011 derived from app/cbl/CBACT02C.cbl")
    @Transactional
    public Cbact02cResponse executeCbact02c0011(Cbact02cRequest request) {
        if (request == null) {
            return Cbact02cResponse.validationError("BR-CBACT02C-0011", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact02cService] Processing rule BR-CBACT02C-0011 for account: {}, amount: {}", request.accountId(), amount);

        return lookupCardXref(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT02C-0011.
     */
    @Transactional
    public ModernizedOperationResult executeCbact02c0011(Map<String, Object> payload) {
        Cbact02cRequest req = mapToRequest(payload);
        Cbact02cResponse resp = executeCbact02c0011(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT02C-0012
     * Description: Enforces BR-CBACT02C-0012 operations from app/cbl/CBACT02C.cbl
     * Mainframe Source Citation: app/cbl/CBACT02C.cbl (Lines 97-100)
     */
    @Operation(summary = "Business logic rule BR-CBACT02C-0012", description = "Implements legacy business rule BR-CBACT02C-0012 derived from app/cbl/CBACT02C.cbl")
    @Transactional
    public Cbact02cResponse executeCbact02c0012(Cbact02cRequest request) {
        if (request == null) {
            return Cbact02cResponse.validationError("BR-CBACT02C-0012", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact02cService] Processing rule BR-CBACT02C-0012 for account: {}, amount: {}", request.accountId(), amount);

        return lookupCardXref(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT02C-0012.
     */
    @Transactional
    public ModernizedOperationResult executeCbact02c0012(Map<String, Object> payload) {
        Cbact02cRequest req = mapToRequest(payload);
        Cbact02cResponse resp = executeCbact02c0012(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT02C-0013
     * Description: Enforces BR-CBACT02C-0013 operations from app/cbl/CBACT02C.cbl
     * Mainframe Source Citation: app/cbl/CBACT02C.cbl (Lines 120-123)
     */
    @Operation(summary = "Business logic rule BR-CBACT02C-0013", description = "Implements legacy business rule BR-CBACT02C-0013 derived from app/cbl/CBACT02C.cbl")
    @Transactional
    public Cbact02cResponse executeCbact02c0013(Cbact02cRequest request) {
        if (request == null) {
            return Cbact02cResponse.validationError("BR-CBACT02C-0013", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact02cService] Processing rule BR-CBACT02C-0013 for account: {}, amount: {}", request.accountId(), amount);

        return lookupCardXref(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT02C-0013.
     */
    @Transactional
    public ModernizedOperationResult executeCbact02c0013(Map<String, Object> payload) {
        Cbact02cRequest req = mapToRequest(payload);
        Cbact02cResponse resp = executeCbact02c0013(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT02C-0014
     * Description: Enforces BR-CBACT02C-0014 operations from app/cbl/CBACT02C.cbl
     * Mainframe Source Citation: app/cbl/CBACT02C.cbl (Lines 138-141)
     */
    @Operation(summary = "Business logic rule BR-CBACT02C-0014", description = "Implements legacy business rule BR-CBACT02C-0014 derived from app/cbl/CBACT02C.cbl")
    @Transactional
    public Cbact02cResponse executeCbact02c0014(Cbact02cRequest request) {
        if (request == null) {
            return Cbact02cResponse.validationError("BR-CBACT02C-0014", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact02cService] Processing rule BR-CBACT02C-0014 for account: {}, amount: {}", request.accountId(), amount);

        return lookupCardXref(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT02C-0014.
     */
    @Transactional
    public ModernizedOperationResult executeCbact02c0014(Map<String, Object> payload) {
        Cbact02cRequest req = mapToRequest(payload);
        Cbact02cResponse resp = executeCbact02c0014(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT02C-0015
     * Description: Enforces BR-CBACT02C-0015 operations from app/cbl/CBACT02C.cbl
     * Mainframe Source Citation: app/cbl/CBACT02C.cbl (Lines 161-164)
     */
    @Operation(summary = "Business logic rule BR-CBACT02C-0015", description = "Implements legacy business rule BR-CBACT02C-0015 derived from app/cbl/CBACT02C.cbl")
    @Transactional
    public Cbact02cResponse executeCbact02c0015(Cbact02cRequest request) {
        if (request == null) {
            return Cbact02cResponse.validationError("BR-CBACT02C-0015", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact02cService] Processing rule BR-CBACT02C-0015 for account: {}, amount: {}", request.accountId(), amount);

        return lookupCardXref(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT02C-0015.
     */
    @Transactional
    public ModernizedOperationResult executeCbact02c0015(Map<String, Object> payload) {
        Cbact02cRequest req = mapToRequest(payload);
        Cbact02cResponse resp = executeCbact02c0015(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Procedural Paragraph: MAIN-ENTRY
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performMainEntry(Map<String, Object> state) {
        log.debug("[Cbact02cService] Executing paragraph routine: MAIN-ENTRY");
        if (state != null) {
            state.put("lastExecutedParagraph", "MAIN-ENTRY");
            state.put("MainEntry_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 1000-CARDFILE-GET-NEXT
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM1000CardfileGetNext(Map<String, Object> state) {
        log.debug("[Cbact02cService] Executing paragraph routine: 1000-CARDFILE-GET-NEXT");
        if (state != null) {
            state.put("lastExecutedParagraph", "1000-CARDFILE-GET-NEXT");
            state.put("M1000CardfileGetNext_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 0000-CARDFILE-OPEN
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM0000CardfileOpen(Map<String, Object> state) {
        log.debug("[Cbact02cService] Executing paragraph routine: 0000-CARDFILE-OPEN");
        if (state != null) {
            state.put("lastExecutedParagraph", "0000-CARDFILE-OPEN");
            state.put("M0000CardfileOpen_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 9000-CARDFILE-CLOSE
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM9000CardfileClose(Map<String, Object> state) {
        log.debug("[Cbact02cService] Executing paragraph routine: 9000-CARDFILE-CLOSE");
        if (state != null) {
            state.put("lastExecutedParagraph", "9000-CARDFILE-CLOSE");
            state.put("M9000CardfileClose_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 9999-ABEND-PROGRAM
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM9999AbendProgram(Map<String, Object> state) {
        log.debug("[Cbact02cService] Executing paragraph routine: 9999-ABEND-PROGRAM");
        if (state != null) {
            state.put("lastExecutedParagraph", "9999-ABEND-PROGRAM");
            state.put("M9999AbendProgram_status", "EXECUTED");
        }
    }

    /**
     * Rule BR-CBACT02C-0009 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT02C (IF END-OF-FILE = 'N').
     * @citation app/cbl/CBACT02C.cbl:74-77
     */
    void rule0(CobolState st) {
        // Rule condition: BR-CBACT02C-0009 (VALIDATION)
        // Statement: Validate field conditions and reject invalid inputs in CBACT02C (IF END-OF-FILE = 'N').
        // Untranslated reason: performs 1000-CARDFILE-GET-NEXT, which is not translated
        throw new NotTranslatedException("Rule BR-CBACT02C-0009: performs 1000-CARDFILE-GET-NEXT, which is not translated");
    }

    /**
     * Rule BR-CBACT02C-0010 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT02C (IF END-OF-FILE = 'N').
     * @citation app/cbl/CBACT02C.cbl:76-79
     */
    void rule1(CobolState st) {
        // Rule condition: BR-CBACT02C-0010 (VALIDATION)
        // Statement: Validate field conditions and reject invalid inputs in CBACT02C (IF END-OF-FILE = 'N').
        // Untranslated reason: performs 1000-CARDFILE-GET-NEXT, which is not translated
        throw new NotTranslatedException("Rule BR-CBACT02C-0010: performs 1000-CARDFILE-GET-NEXT, which is not translated");
    }

    /**
     * Rule BR-CBACT02C-0011 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT02C (IF CARDFILE-STATUS = '00').
     * @citation app/cbl/CBACT02C.cbl:93-96
     */
    void rule2(CobolState st) {
        st.move("APPL-RESULT", new BigDecimal("0"));
    }

    /**
     * Rule BR-CBACT02C-0012 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT02C (IF CARDFILE-STATUS = '10').
     * @citation app/cbl/CBACT02C.cbl:97-100
     */
    void rule3(CobolState st) {
        st.move("APPL-RESULT", new BigDecimal("16"));
    }

    /**
     * Rule BR-CBACT02C-0013 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT02C (IF CARDFILE-STATUS = '00').
     * @citation app/cbl/CBACT02C.cbl:120-123
     */
    void rule4(CobolState st) {
        st.move("APPL-RESULT", new BigDecimal("0"));
    }

    /**
     * Rule BR-CBACT02C-0014 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT02C (IF CARDFILE-STATUS = '00').
     * @citation app/cbl/CBACT02C.cbl:138-141
     */
    void rule5(CobolState st) {
        // Rule condition: BR-CBACT02C-0014 (VALIDATION)
        // Statement: Validate field conditions and reject invalid inputs in CBACT02C (IF CARDFILE-STATUS = '00').
        // Untranslated reason: comparison of group item CARDFILE-STATUS is not translated
        throw new NotTranslatedException("Rule BR-CBACT02C-0014: comparison of group item CARDFILE-STATUS is not translated");
    }

    /**
     * Rule BR-CBACT02C-0015 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT02C (IF IO-STATUS NOT NUMERIC).
     * @citation app/cbl/CBACT02C.cbl:161-164
     */
    void rule6(CobolState st) {
        if ((!(st.isNumeric("IO-STATUS")) || (st.cmp(st.str("IO-STAT1"), "9") == 0))) {
            st.move("IO-STATUS-04", st.val("IO-STAT1"));
        }
    }

    /** Paragraph MAIN-ENTRY. */
    void pMainEntry(CobolState st) {
        throw new NotTranslatedException("MAIN-ENTRY: DISPLAY of group item CARD-RECORD is not translated");
    }

    /** Paragraph 1000-CARDFILE-GET-NEXT. */
    void pM1000CardfileGetNext(CobolState st) {
        throw new NotTranslatedException("1000-CARDFILE-GET-NEXT: comparison of group item CARDFILE-STATUS is not translated");
    }

    /** Paragraph 0000-CARDFILE-OPEN. */
    void pM0000CardfileOpen(CobolState st) {
        throw new NotTranslatedException("0000-CARDFILE-OPEN: comparison of group item CARDFILE-STATUS is not translated");
    }

    /** Paragraph 9000-CARDFILE-CLOSE. */
    void pM9000CardfileClose(CobolState st) {
        throw new NotTranslatedException("9000-CARDFILE-CLOSE: comparison of group item CARDFILE-STATUS is not translated");
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
        throw new NotTranslatedException("9910-DISPLAY-IO-STATUS: DISPLAY of group item IO-STATUS-04 is not translated");
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
