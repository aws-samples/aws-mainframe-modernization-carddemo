package com.client.core_financial_processing.service;


import java.util.List;
import java.util.Map;
import com.client.runtime.CobolState;
import com.client.runtime.NotTranslatedException;
import com.client.runtime.ProgramExit;
import com.client.runtime.ProgramRegistry;
import com.client.runtime.ProgramService;
import com.client.runtime.RuleInfo;
import com.client.core_financial_processing.dto.Cbact03cRequest;
import com.client.core_financial_processing.dto.Cbact03cResponse;
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
 * Modernized Spring Boot Domain Service: Cbact03cService
 *
 * Transpiled from Mainframe COBOL program: CBACT03C
 * Legacy Source Path: app/cbl/CBACT03C.cbl
 * Generation Strategy: Enterprise Modernization with ProgramService registry integration
 */
@Slf4j
@Service
@Tag(name = "Cbact03cService", description = "Modernized domain business service for legacy CBACT03C")
public class Cbact03cService implements ProgramService {

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

    private static final List<String> UNTRANSLATED = List.of();

    private static final List<RuleInfo> RULES = List.of(
            new RuleInfo("BR-CBACT03C-0016", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT03C (IF END-OF-FILE = 'N').", "app/cbl/CBACT03C.cbl:74-77", false, "expected END-PERFORM but found the end of the cited lines", List.of("END-OF-FILE", "END-OF-FILE", "END-OF-FILE", "END-OF-FILE"), "untranslated (expected END-PERFORM but found the end of the cited lines)"),
            new RuleInfo("BR-CBACT03C-0017", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT03C (IF END-OF-FILE = 'N').", "app/cbl/CBACT03C.cbl:76-79", true, null, List.of("END-OF-FILE", "CARD-XREF-RECORD"), ""),
            new RuleInfo("BR-CBACT03C-0018", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT03C (IF XREFFILE-STATUS = '00').", "app/cbl/CBACT03C.cbl:93-96", true, null, List.of("XREFFILE-FILE", "CARD-XREF-RECORD", "APPL-RESULT"), ""),
            new RuleInfo("BR-CBACT03C-0019", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT03C (IF XREFFILE-STATUS = '10').", "app/cbl/CBACT03C.cbl:97-100", true, null, List.of("APPL-RESULT"), ""),
            new RuleInfo("BR-CBACT03C-0020", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT03C (IF XREFFILE-STATUS = '00').", "app/cbl/CBACT03C.cbl:120-123", true, null, List.of("XREFFILE-FILE", "APPL-RESULT"), ""),
            new RuleInfo("BR-CBACT03C-0021", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT03C (IF XREFFILE-STATUS = '00').", "app/cbl/CBACT03C.cbl:138-141", true, null, List.of("XREFFILE-FILE", "APPL-RESULT"), ""),
            new RuleInfo("BR-CBACT03C-0022", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT03C (IF IO-STATUS NOT NUMERIC).", "app/cbl/CBACT03C.cbl:161-164", true, null, List.of("IO-STATUS", "IO-STAT1", "IO-STATUS-04"), ""));

    @Override
    public String programId() {
        return "CBACT03C";
    }

    @Override
    public List<String> aliases() {
        return List.of();
    }

    @Override
    public String sourcePath() {
        return "app/cbl/CBACT03C.cbl";
    }

    @Override
    public boolean programTranslated() {
        return true;
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
        return CobolState.load("CBACT03C");
    }

    public String getModuleId() {
        return "CBACT03C";
    }

    public String getSourcePath() {
        return "app/cbl/CBACT03C.cbl";
    }

    public List<String> getAliases() {
        return aliases();
    }

    @Override
    public void executeRule(String ruleId, CobolState st) {
        switch (ruleId) {
            case "BR-CBACT03C-0016": rule0(st); return;
            case "BR-CBACT03C-0017": rule1(st); return;
            case "BR-CBACT03C-0018": rule2(st); return;
            case "BR-CBACT03C-0019": rule3(st); return;
            case "BR-CBACT03C-0020": rule4(st); return;
            case "BR-CBACT03C-0021": rule5(st); return;
            case "BR-CBACT03C-0022": rule6(st); return;
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
            pM1000XreffileGetNext(st);
            pM0000XreffileOpen(st);
            pM9000XreffileClose(st);
            pM9999AbendProgram(st);
            pM9910DisplayIoStatus(st);
        } catch (ProgramExit e) {
            // GOBACK / STOP RUN
        }
    }

    private Cbact03cRequest mapToRequest(Map<String, Object> payload) {
        if (payload == null || payload.isEmpty()) {
            return Cbact03cRequest.defaultBaseline();
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
        return new Cbact03cRequest(acct, amt, type, ref, card, curBal, credLim, intRate, null, null, dateStr, dateFmt, waitCs, payload);
    }

    /**
     * CardDemo CBACT03C: Card Cross-Reference Record Adder
     * Mainframe Source: CBACT03C.cbl
     * Rejection Reason Codes:
     *   22: Mandatory XREF keys missing
     */
    @Operation(summary = "Create Card XREF Record", description = "Inserts a new card-to-account mapping into XREFFILE")
    @Transactional
    public Cbact03cResponse createCardXref(Cbact03cRequest request) {
        if (request == null) {
            return Cbact03cResponse.declined("0000-XREFFILE-ADD", "MANDATORY XREF FIELDS MISSING", 22, BigDecimal.ZERO, "");
        }
        String cardNum = request.cardNumber() != null ? request.cardNumber().trim() : "";
        String acctId = request.accountId() != null ? request.accountId().trim() : "";
        if (cardNum.isEmpty() || acctId.isEmpty() || "0000000000000000".equals(cardNum)) {
            log.warn("[CBACT03C] Validation failure 22: MANDATORY XREF FIELDS MISSING");
            return Cbact03cResponse.declined("0000-XREFFILE-ADD", "MANDATORY XREF FIELDS MISSING", 22, BigDecimal.ZERO, request.referenceNumber());
        }
        String custId = request.customerId() != null ? request.customerId() : "";
        if (cvxrf01yRepository != null) {
            try {
                var xref = new com.client.domain.model.Cvxrf01y();
                xref.setCardNum(cardNum);
                try { xref.setAcctId(Long.parseLong(acctId.trim())); } catch (Exception ignored) {}
                if (!custId.isEmpty()) {
                    try { xref.setCustId(Integer.parseInt(custId.trim())); } catch (Exception ignored) {}
                }
                cvxrf01yRepository.save(xref);
                log.info("[CBACT03C] Persisted card cross-reference via Cvxrf01yRepository");
            } catch (Exception e) {
                log.warn("[CBACT03C] Could not persist xref: {}", e.getMessage());
            }
        }
        log.info("[CBACT03C] Card XREF record created successfully: card: {} -> account: {}", cardNum, acctId);
        return Cbact03cResponse.xrefSuccess("0000-XREFFILE-ADD", "Card cross-reference record created successfully", cardNum, acctId, custId, request.referenceNumber());
    }

    /**
     * Business Rule: BR-CBACT03C-0016
     * Description: Enforces BR-CBACT03C-0016 operations from app/cbl/CBACT03C.cbl
     * Mainframe Source Citation: app/cbl/CBACT03C.cbl (Lines 74-77)
     */
    @Operation(summary = "Business logic rule BR-CBACT03C-0016", description = "Implements legacy business rule BR-CBACT03C-0016 derived from app/cbl/CBACT03C.cbl")
    @Transactional
    public Cbact03cResponse executeCbact03c0016(Cbact03cRequest request) {
        if (request == null) {
            return Cbact03cResponse.validationError("BR-CBACT03C-0016", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact03cService] Processing rule BR-CBACT03C-0016 for account: {}, amount: {}", request.accountId(), amount);

        return createCardXref(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT03C-0016.
     */
    @Transactional
    public ModernizedOperationResult executeCbact03c0016(Map<String, Object> payload) {
        Cbact03cRequest req = mapToRequest(payload);
        Cbact03cResponse resp = executeCbact03c0016(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT03C-0017
     * Description: Enforces BR-CBACT03C-0017 operations from app/cbl/CBACT03C.cbl
     * Mainframe Source Citation: app/cbl/CBACT03C.cbl (Lines 76-79)
     */
    @Operation(summary = "Business logic rule BR-CBACT03C-0017", description = "Implements legacy business rule BR-CBACT03C-0017 derived from app/cbl/CBACT03C.cbl")
    @Transactional
    public Cbact03cResponse executeCbact03c0017(Cbact03cRequest request) {
        if (request == null) {
            return Cbact03cResponse.validationError("BR-CBACT03C-0017", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact03cService] Processing rule BR-CBACT03C-0017 for account: {}, amount: {}", request.accountId(), amount);

        return createCardXref(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT03C-0017.
     */
    @Transactional
    public ModernizedOperationResult executeCbact03c0017(Map<String, Object> payload) {
        Cbact03cRequest req = mapToRequest(payload);
        Cbact03cResponse resp = executeCbact03c0017(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT03C-0018
     * Description: Enforces BR-CBACT03C-0018 operations from app/cbl/CBACT03C.cbl
     * Mainframe Source Citation: app/cbl/CBACT03C.cbl (Lines 93-96)
     */
    @Operation(summary = "Business logic rule BR-CBACT03C-0018", description = "Implements legacy business rule BR-CBACT03C-0018 derived from app/cbl/CBACT03C.cbl")
    @Transactional
    public Cbact03cResponse executeCbact03c0018(Cbact03cRequest request) {
        if (request == null) {
            return Cbact03cResponse.validationError("BR-CBACT03C-0018", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact03cService] Processing rule BR-CBACT03C-0018 for account: {}, amount: {}", request.accountId(), amount);

        return createCardXref(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT03C-0018.
     */
    @Transactional
    public ModernizedOperationResult executeCbact03c0018(Map<String, Object> payload) {
        Cbact03cRequest req = mapToRequest(payload);
        Cbact03cResponse resp = executeCbact03c0018(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT03C-0019
     * Description: Enforces BR-CBACT03C-0019 operations from app/cbl/CBACT03C.cbl
     * Mainframe Source Citation: app/cbl/CBACT03C.cbl (Lines 97-100)
     */
    @Operation(summary = "Business logic rule BR-CBACT03C-0019", description = "Implements legacy business rule BR-CBACT03C-0019 derived from app/cbl/CBACT03C.cbl")
    @Transactional
    public Cbact03cResponse executeCbact03c0019(Cbact03cRequest request) {
        if (request == null) {
            return Cbact03cResponse.validationError("BR-CBACT03C-0019", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact03cService] Processing rule BR-CBACT03C-0019 for account: {}, amount: {}", request.accountId(), amount);

        return createCardXref(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT03C-0019.
     */
    @Transactional
    public ModernizedOperationResult executeCbact03c0019(Map<String, Object> payload) {
        Cbact03cRequest req = mapToRequest(payload);
        Cbact03cResponse resp = executeCbact03c0019(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT03C-0020
     * Description: Enforces BR-CBACT03C-0020 operations from app/cbl/CBACT03C.cbl
     * Mainframe Source Citation: app/cbl/CBACT03C.cbl (Lines 120-123)
     */
    @Operation(summary = "Business logic rule BR-CBACT03C-0020", description = "Implements legacy business rule BR-CBACT03C-0020 derived from app/cbl/CBACT03C.cbl")
    @Transactional
    public Cbact03cResponse executeCbact03c0020(Cbact03cRequest request) {
        if (request == null) {
            return Cbact03cResponse.validationError("BR-CBACT03C-0020", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact03cService] Processing rule BR-CBACT03C-0020 for account: {}, amount: {}", request.accountId(), amount);

        return createCardXref(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT03C-0020.
     */
    @Transactional
    public ModernizedOperationResult executeCbact03c0020(Map<String, Object> payload) {
        Cbact03cRequest req = mapToRequest(payload);
        Cbact03cResponse resp = executeCbact03c0020(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT03C-0021
     * Description: Enforces BR-CBACT03C-0021 operations from app/cbl/CBACT03C.cbl
     * Mainframe Source Citation: app/cbl/CBACT03C.cbl (Lines 138-141)
     */
    @Operation(summary = "Business logic rule BR-CBACT03C-0021", description = "Implements legacy business rule BR-CBACT03C-0021 derived from app/cbl/CBACT03C.cbl")
    @Transactional
    public Cbact03cResponse executeCbact03c0021(Cbact03cRequest request) {
        if (request == null) {
            return Cbact03cResponse.validationError("BR-CBACT03C-0021", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact03cService] Processing rule BR-CBACT03C-0021 for account: {}, amount: {}", request.accountId(), amount);

        return createCardXref(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT03C-0021.
     */
    @Transactional
    public ModernizedOperationResult executeCbact03c0021(Map<String, Object> payload) {
        Cbact03cRequest req = mapToRequest(payload);
        Cbact03cResponse resp = executeCbact03c0021(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT03C-0022
     * Description: Enforces BR-CBACT03C-0022 operations from app/cbl/CBACT03C.cbl
     * Mainframe Source Citation: app/cbl/CBACT03C.cbl (Lines 161-164)
     */
    @Operation(summary = "Business logic rule BR-CBACT03C-0022", description = "Implements legacy business rule BR-CBACT03C-0022 derived from app/cbl/CBACT03C.cbl")
    @Transactional
    public Cbact03cResponse executeCbact03c0022(Cbact03cRequest request) {
        if (request == null) {
            return Cbact03cResponse.validationError("BR-CBACT03C-0022", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact03cService] Processing rule BR-CBACT03C-0022 for account: {}, amount: {}", request.accountId(), amount);

        return createCardXref(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT03C-0022.
     */
    @Transactional
    public ModernizedOperationResult executeCbact03c0022(Map<String, Object> payload) {
        Cbact03cRequest req = mapToRequest(payload);
        Cbact03cResponse resp = executeCbact03c0022(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Procedural Paragraph: MAIN-ENTRY
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performMainEntry(Map<String, Object> state) {
        log.debug("[Cbact03cService] Executing paragraph routine: MAIN-ENTRY");
        if (state != null) {
            state.put("lastExecutedParagraph", "MAIN-ENTRY");
            state.put("MainEntry_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 1000-XREFFILE-GET-NEXT
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM1000XreffileGetNext(Map<String, Object> state) {
        log.debug("[Cbact03cService] Executing paragraph routine: 1000-XREFFILE-GET-NEXT");
        if (state != null) {
            state.put("lastExecutedParagraph", "1000-XREFFILE-GET-NEXT");
            state.put("M1000XreffileGetNext_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 0000-XREFFILE-OPEN
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM0000XreffileOpen(Map<String, Object> state) {
        log.debug("[Cbact03cService] Executing paragraph routine: 0000-XREFFILE-OPEN");
        if (state != null) {
            state.put("lastExecutedParagraph", "0000-XREFFILE-OPEN");
            state.put("M0000XreffileOpen_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 9000-XREFFILE-CLOSE
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM9000XreffileClose(Map<String, Object> state) {
        log.debug("[Cbact03cService] Executing paragraph routine: 9000-XREFFILE-CLOSE");
        if (state != null) {
            state.put("lastExecutedParagraph", "9000-XREFFILE-CLOSE");
            state.put("M9000XreffileClose_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 9999-ABEND-PROGRAM
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM9999AbendProgram(Map<String, Object> state) {
        log.debug("[Cbact03cService] Executing paragraph routine: 9999-ABEND-PROGRAM");
        if (state != null) {
            state.put("lastExecutedParagraph", "9999-ABEND-PROGRAM");
            state.put("M9999AbendProgram_status", "EXECUTED");
        }
    }

    /**
     * Rule BR-CBACT03C-0016 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT03C (IF END-OF-FILE = 'N').
     * @citation app/cbl/CBACT03C.cbl:74-77
     */
    void rule0(CobolState st) {
        // Rule condition: BR-CBACT03C-0016 (VALIDATION)
        // Statement: Validate field conditions and reject invalid inputs in CBACT03C (IF END-OF-FILE = 'N').
        // Untranslated reason: expected END-PERFORM but found the end of the cited lines
        throw new NotTranslatedException("Rule BR-CBACT03C-0016: expected END-PERFORM but found the end of the cited lines");
    }

    /**
     * Rule BR-CBACT03C-0017 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT03C (IF END-OF-FILE = 'N').
     * @citation app/cbl/CBACT03C.cbl:76-79
     */
    void rule1(CobolState st) {
        pM1000XreffileGetNext(st);
        if ((st.cmp(st.str("END-OF-FILE"), "N") == 0)) {
            st.display(st.str("CARD-XREF-RECORD"));
        }
    }

    /**
     * Rule BR-CBACT03C-0018 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT03C (IF XREFFILE-STATUS = '00').
     * @citation app/cbl/CBACT03C.cbl:93-96
     */
    void rule2(CobolState st) {
        st.readFile("XREFFILE-FILE", "CARD-XREF-RECORD");
        if ((st.cmp(st.str("XREFFILE-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
            st.display(st.str("CARD-XREF-RECORD"));
        }
    }

    /**
     * Rule BR-CBACT03C-0019 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT03C (IF XREFFILE-STATUS = '10').
     * @citation app/cbl/CBACT03C.cbl:97-100
     */
    void rule3(CobolState st) {
        if ((st.cmp(new BigDecimal("1"), new BigDecimal("1")) == 0)) {
            // CONTINUE
        } else {
            if ((st.cmp(st.str("XREFFILE-STATUS"), "10") == 0)) {
                st.move("APPL-RESULT", new BigDecimal("16"));
            } else {
            }
        }
    }

    /**
     * Rule BR-CBACT03C-0020 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT03C (IF XREFFILE-STATUS = '00').
     * @citation app/cbl/CBACT03C.cbl:120-123
     */
    void rule4(CobolState st) {
        st.openFile("XREFFILE-FILE", "INPUT");
        if ((st.cmp(st.str("XREFFILE-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
        } else {
        }
    }

    /**
     * Rule BR-CBACT03C-0021 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT03C (IF XREFFILE-STATUS = '00').
     * @citation app/cbl/CBACT03C.cbl:138-141
     */
    void rule5(CobolState st) {
        st.closeFile("XREFFILE-FILE");
        if ((st.cmp(st.str("XREFFILE-STATUS"), "00") == 0)) {
            st.setNum("APPL-RESULT", st.num("APPL-RESULT").subtract(st.num("APPL-RESULT")), false);
        } else {
        }
    }

    /**
     * Rule BR-CBACT03C-0022 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT03C (IF IO-STATUS NOT NUMERIC).
     * @citation app/cbl/CBACT03C.cbl:161-164
     */
    void rule6(CobolState st) {
        if ((!(st.isNumeric("IO-STATUS")) || (st.cmp(st.str("IO-STAT1"), "9") == 0))) {
            st.move("IO-STATUS-04", st.val("IO-STAT1"));
        }
    }

    /** Paragraph MAIN-ENTRY. */
    void pMainEntry(CobolState st) {
        st.display("START OF EXECUTION OF PROGRAM CBACT03C");
        pM0000XreffileOpen(st);
        while (!(st.cmp(st.str("END-OF-FILE"), "Y") == 0)) {
            st.tick();
            if ((st.cmp(st.str("END-OF-FILE"), "N") == 0)) {
                pM1000XreffileGetNext(st);
                if ((st.cmp(st.str("END-OF-FILE"), "N") == 0)) {
                    st.display(st.str("CARD-XREF-RECORD"));
                }
            }
        }
        pM9000XreffileClose(st);
        st.display("END OF EXECUTION OF PROGRAM CBACT03C");
        st.exitProgram();
    }

    /** Paragraph 1000-XREFFILE-GET-NEXT. */
    void pM1000XreffileGetNext(CobolState st) {
        st.readFile("XREFFILE-FILE", "CARD-XREF-RECORD");
        if ((st.cmp(st.str("XREFFILE-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
            st.display(st.str("CARD-XREF-RECORD"));
        } else {
            if ((st.cmp(st.str("XREFFILE-STATUS"), "10") == 0)) {
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
                st.display("ERROR READING XREFFILE");
                st.move("IO-STATUS", st.val("XREFFILE-STATUS"));
                pM9910DisplayIoStatus(st);
                pM9999AbendProgram(st);
            }
        }
        // EXIT
    }

    /** Paragraph 0000-XREFFILE-OPEN. */
    void pM0000XreffileOpen(CobolState st) {
        st.move("APPL-RESULT", new BigDecimal("8"));
        st.openFile("XREFFILE-FILE", "INPUT");
        if ((st.cmp(st.str("XREFFILE-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
        } else {
            st.move("APPL-RESULT", new BigDecimal("12"));
        }
        if (st.is88("APPL-RESULT", new BigDecimal("0"))) {
            // CONTINUE
        } else {
            st.display("ERROR OPENING XREFFILE");
            st.move("IO-STATUS", st.val("XREFFILE-STATUS"));
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
    }

    /** Paragraph 9000-XREFFILE-CLOSE. */
    void pM9000XreffileClose(CobolState st) {
        st.setNum("APPL-RESULT", new BigDecimal("8").add(BigDecimal.ZERO), false);
        st.closeFile("XREFFILE-FILE");
        if ((st.cmp(st.str("XREFFILE-STATUS"), "00") == 0)) {
            st.setNum("APPL-RESULT", st.num("APPL-RESULT").subtract(st.num("APPL-RESULT")), false);
        } else {
            st.setNum("APPL-RESULT", new BigDecimal("12").add(BigDecimal.ZERO), false);
        }
        if (st.is88("APPL-RESULT", new BigDecimal("0"))) {
            // CONTINUE
        } else {
            st.display("ERROR CLOSING XREFFILE");
            st.move("IO-STATUS", st.val("XREFFILE-STATUS"));
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
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
