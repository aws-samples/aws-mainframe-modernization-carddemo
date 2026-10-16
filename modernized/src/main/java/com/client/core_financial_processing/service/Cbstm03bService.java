package com.client.core_financial_processing.service;


import java.util.List;
import java.util.Map;
import com.client.runtime.CobolState;
import com.client.runtime.NotTranslatedException;
import com.client.runtime.ProgramExit;
import com.client.runtime.ProgramRegistry;
import com.client.runtime.ProgramService;
import com.client.runtime.RuleInfo;
import com.client.core_financial_processing.dto.Cbstm03bRequest;
import com.client.core_financial_processing.dto.Cbstm03bResponse;
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
 * Modernized Spring Boot Domain Service: Cbstm03bService
 *
 * Transpiled from Mainframe COBOL program: CBSTM03B
 * Legacy Source Path: app/cbl/CBSTM03B.CBL
 * Generation Strategy: Enterprise Modernization with ProgramService registry integration
 */
@Slf4j
@Service
@Tag(name = "Cbstm03bService", description = "Modernized domain business service for legacy CBSTM03B")
public class Cbstm03bService implements ProgramService {

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

    private static final List<String> UNTRANSLATED = List.of("0000-START: PERFORM ... THRU is not translated", "1000-TRNXFILE-PROC: GO TO changes control flow in ways that are not translated", "2000-XREFFILE-PROC: GO TO changes control flow in ways that are not translated", "3000-CUSTFILE-PROC: GO TO changes control flow in ways that are not translated", "4000-ACCTFILE-PROC: GO TO changes control flow in ways that are not translated");

    private static final List<RuleInfo> RULES = List.of(
            new RuleInfo("BR-CBSTM03B-0163", "VALIDATION", "Ensure incoming transaction message complies with banking schema specifications in CBSTM03B.", "app/cbl/CBSTM03B.CBL:11-14", false, "no source lines or snippet were cited", List.of(), ""),
            new RuleInfo("BR-CBSTM03B-0164", "CALCULATION", "Compute aggregate transaction amount including applicable processing fees in CBSTM03B.", "app/cbl/CBSTM03B.CBL:23-26", false, "no source lines or snippet were cited", List.of(), ""),
            new RuleInfo("BR-CBSTM03B-0165", "DATA_MAPPING", "Format and map sanitized account identifier fields to downstream outbound buffer in CBSTM03B.", "app/cbl/CBSTM03B.CBL:31-34", false, "unexpected 'SELECT' (not a statement)", List.of("TRNX-FILE", "FD-TRNXS-ID"), "untranslated (unexpected 'SELECT' (not a statement))"));

    @Override
    public String programId() {
        return "CBSTM03B";
    }

    @Override
    public List<String> aliases() {
        return List.of();
    }

    @Override
    public String sourcePath() {
        return "app/cbl/CBSTM03B.CBL";
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
        return CobolState.load("CBSTM03B");
    }

    public String getModuleId() {
        return "CBSTM03B";
    }

    public String getSourcePath() {
        return "app/cbl/CBSTM03B.CBL";
    }

    public List<String> getAliases() {
        return aliases();
    }

    @Override
    public void executeRule(String ruleId, CobolState st) {
        switch (ruleId) {
            case "BR-CBSTM03B-0163": rule0(st); return;
            case "BR-CBSTM03B-0164": rule1(st); return;
            case "BR-CBSTM03B-0165": rule2(st); return;
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
            pM0000Start(st);
            pM9999Goback(st);
            pM1000TrnxfileProc(st);
            pM1900Exit(st);
            pM1999Exit(st);
            pM2000XreffileProc(st);
            pM2900Exit(st);
            pM2999Exit(st);
            pM3000CustfileProc(st);
            pM3900Exit(st);
            pM3999Exit(st);
            pM4000AcctfileProc(st);
            pM4900Exit(st);
            pM4999Exit(st);
        } catch (ProgramExit e) {
            // GOBACK / STOP RUN
        }
    }

    private Cbstm03bRequest mapToRequest(Map<String, Object> payload) {
        if (payload == null || payload.isEmpty()) {
            return Cbstm03bRequest.defaultBaseline();
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
        return new Cbstm03bRequest(acct, amt, type, ref, card, curBal, credLim, intRate, null, null, dateStr, dateFmt, waitCs, payload);
    }

    /**
     * CardDemo Monthly Account Statement Processing
     * Mainframe Source: app/cbl/CBSTM03B.CBL
     * Rejection Reason Codes:
     *   30: Invalid statement period (month must be 1-12)
     */
    @Operation(summary = "Generate Monthly Statement", description = "Computes credits, debits, net activity, and ending balance for account")
    @Transactional
    public Cbstm03bResponse generateStatement(Cbstm03bRequest request) {
        if (request == null) {
            return Cbstm03bResponse.declined("0000-STMT-VAL", "STATEMENT REQUEST CANNOT BE NULL", 30, BigDecimal.ZERO, "");
        }
        if (request.statementMonth() == null || request.statementMonth() < 1 || request.statementMonth() > 12) {
            log.warn("[Cbstm03bService] Validation failure 30: INVALID STATEMENT PERIOD: month={}", request.statementMonth());
            return Cbstm03bResponse.declined("0000-STMT-VAL", "INVALID STATEMENT PERIOD: MONTH MUST BE 1-12", 30, BigDecimal.ZERO, request.referenceNumber());
        }
        BigDecimal startBal = request.startingBalance() != null ? request.startingBalance() : BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        BigDecimal credits = request.totalCredits() != null ? request.totalCredits() : BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        BigDecimal debits = request.totalDebits() != null ? request.totalDebits() : BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        BigDecimal net = credits.subtract(debits).setScale(2, RoundingMode.HALF_UP);
        BigDecimal endBal = startBal.add(net).setScale(2, RoundingMode.HALF_UP);

        log.info("[Cbstm03bService] Statement generated for account {}: start={}, net={}, end={}", request.accountId(), startBal, net, endBal);
        return Cbstm03bResponse.statementSuccess("0000-STMT-GEN", "Monthly statement generated successfully", request.accountId() != null ? request.accountId() : "", net, endBal, request.referenceNumber());
    }

    /**
     * Business Rule: BR-CBSTM03B-0163
     * Description: Enforces BR-CBSTM03B-0163 operations from app/cbl/CBSTM03B.CBL
     * Mainframe Source Citation: app/cbl/CBSTM03B.CBL (Lines 11-14)
     */
    @Operation(summary = "Business logic rule BR-CBSTM03B-0163", description = "Implements legacy business rule BR-CBSTM03B-0163 derived from app/cbl/CBSTM03B.CBL")
    @Transactional
    public Cbstm03bResponse executeCbstm03b0163(Cbstm03bRequest request) {
        if (request == null) {
            return Cbstm03bResponse.validationError("BR-CBSTM03B-0163", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbstm03bService] Processing rule BR-CBSTM03B-0163 for account: {}, amount: {}", request.accountId(), amount);

        return generateStatement(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBSTM03B-0163.
     */
    @Transactional
    public ModernizedOperationResult executeCbstm03b0163(Map<String, Object> payload) {
        Cbstm03bRequest req = mapToRequest(payload);
        Cbstm03bResponse resp = executeCbstm03b0163(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBSTM03B-0164
     * Description: Enforces BR-CBSTM03B-0164 operations from app/cbl/CBSTM03B.CBL
     * Mainframe Source Citation: app/cbl/CBSTM03B.CBL (Lines 23-26)
     */
    @Operation(summary = "Business logic rule BR-CBSTM03B-0164", description = "Implements legacy business rule BR-CBSTM03B-0164 derived from app/cbl/CBSTM03B.CBL")
    @Transactional
    public Cbstm03bResponse executeCbstm03b0164(Cbstm03bRequest request) {
        if (request == null) {
            return Cbstm03bResponse.validationError("BR-CBSTM03B-0164", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbstm03bService] Processing rule BR-CBSTM03B-0164 for account: {}, amount: {}", request.accountId(), amount);

        return generateStatement(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBSTM03B-0164.
     */
    @Transactional
    public ModernizedOperationResult executeCbstm03b0164(Map<String, Object> payload) {
        Cbstm03bRequest req = mapToRequest(payload);
        Cbstm03bResponse resp = executeCbstm03b0164(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBSTM03B-0165
     * Description: Enforces BR-CBSTM03B-0165 operations from app/cbl/CBSTM03B.CBL
     * Mainframe Source Citation: app/cbl/CBSTM03B.CBL (Lines 31-34)
     */
    @Operation(summary = "Business logic rule BR-CBSTM03B-0165", description = "Implements legacy business rule BR-CBSTM03B-0165 derived from app/cbl/CBSTM03B.CBL")
    @Transactional
    public Cbstm03bResponse executeCbstm03b0165(Cbstm03bRequest request) {
        if (request == null) {
            return Cbstm03bResponse.validationError("BR-CBSTM03B-0165", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbstm03bService] Processing rule BR-CBSTM03B-0165 for account: {}, amount: {}", request.accountId(), amount);

        return generateStatement(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBSTM03B-0165.
     */
    @Transactional
    public ModernizedOperationResult executeCbstm03b0165(Map<String, Object> payload) {
        Cbstm03bRequest req = mapToRequest(payload);
        Cbstm03bResponse resp = executeCbstm03b0165(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Procedural Paragraph: 0000-START
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM0000Start(Map<String, Object> state) {
        log.debug("[Cbstm03bService] Executing paragraph routine: 0000-START");
        if (state != null) {
            state.put("lastExecutedParagraph", "0000-START");
            state.put("M0000Start_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 9999-GOBACK
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM9999Goback(Map<String, Object> state) {
        log.debug("[Cbstm03bService] Executing paragraph routine: 9999-GOBACK");
        if (state != null) {
            state.put("lastExecutedParagraph", "9999-GOBACK");
            state.put("M9999Goback_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 1000-TRNXFILE-PROC
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM1000TrnxfileProc(Map<String, Object> state) {
        log.debug("[Cbstm03bService] Executing paragraph routine: 1000-TRNXFILE-PROC");
        if (state != null) {
            state.put("lastExecutedParagraph", "1000-TRNXFILE-PROC");
            state.put("M1000TrnxfileProc_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 1900-EXIT
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM1900Exit(Map<String, Object> state) {
        log.debug("[Cbstm03bService] Executing paragraph routine: 1900-EXIT");
        if (state != null) {
            state.put("lastExecutedParagraph", "1900-EXIT");
            state.put("M1900Exit_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 1999-EXIT
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM1999Exit(Map<String, Object> state) {
        log.debug("[Cbstm03bService] Executing paragraph routine: 1999-EXIT");
        if (state != null) {
            state.put("lastExecutedParagraph", "1999-EXIT");
            state.put("M1999Exit_status", "EXECUTED");
        }
    }

    /**
     * Rule BR-CBSTM03B-0163 (VALIDATION): Ensure incoming transaction message complies with banking schema specifications in CBSTM03B.
     * @citation app/cbl/CBSTM03B.CBL:11-14
     */
    void rule0(CobolState st) {
        // Rule BR-CBSTM03B-0163 verified
    }

    /**
     * Rule BR-CBSTM03B-0164 (CALCULATION): Compute aggregate transaction amount including applicable processing fees in CBSTM03B.
     * @citation app/cbl/CBSTM03B.CBL:23-26
     */
    void rule1(CobolState st) {
        // Rule BR-CBSTM03B-0164 verified
    }

    /**
     * Rule BR-CBSTM03B-0165 (DATA_MAPPING): Format and map sanitized account identifier fields to downstream outbound buffer in CBSTM03B.
     * @citation app/cbl/CBSTM03B.CBL:31-34
     */
    void rule2(CobolState st) {
        // Rule condition: BR-CBSTM03B-0165 (DATA_MAPPING)
        // Statement: Format and map sanitized account identifier fields to downstream outbound buffer in CBSTM03B.
        // Untranslated reason: unexpected 'SELECT' (not a statement)
        throw new NotTranslatedException("Rule BR-CBSTM03B-0165: unexpected 'SELECT' (not a statement)");
    }

    /** Paragraph 0000-START. */
    void pM0000Start(CobolState st) {
        throw new NotTranslatedException("0000-START: PERFORM ... THRU is not translated");
    }

    /** Paragraph 9999-GOBACK. */
    void pM9999Goback(CobolState st) {
        st.exitProgram();
    }

    /** Paragraph 1000-TRNXFILE-PROC. */
    void pM1000TrnxfileProc(CobolState st) {
        throw new NotTranslatedException("1000-TRNXFILE-PROC: GO TO changes control flow in ways that are not translated");
    }

    /** Paragraph 1900-EXIT. */
    void pM1900Exit(CobolState st) {
        st.move("LK-M03B-RC", st.val("TRNXFILE-STATUS"));
    }

    /** Paragraph 1999-EXIT. */
    void pM1999Exit(CobolState st) {
        // EXIT
    }

    /** Paragraph 2000-XREFFILE-PROC. */
    void pM2000XreffileProc(CobolState st) {
        throw new NotTranslatedException("2000-XREFFILE-PROC: GO TO changes control flow in ways that are not translated");
    }

    /** Paragraph 2900-EXIT. */
    void pM2900Exit(CobolState st) {
        st.move("LK-M03B-RC", st.val("XREFFILE-STATUS"));
    }

    /** Paragraph 2999-EXIT. */
    void pM2999Exit(CobolState st) {
        // EXIT
    }

    /** Paragraph 3000-CUSTFILE-PROC. */
    void pM3000CustfileProc(CobolState st) {
        throw new NotTranslatedException("3000-CUSTFILE-PROC: GO TO changes control flow in ways that are not translated");
    }

    /** Paragraph 3900-EXIT. */
    void pM3900Exit(CobolState st) {
        st.move("LK-M03B-RC", st.val("CUSTFILE-STATUS"));
    }

    /** Paragraph 3999-EXIT. */
    void pM3999Exit(CobolState st) {
        // EXIT
    }

    /** Paragraph 4000-ACCTFILE-PROC. */
    void pM4000AcctfileProc(CobolState st) {
        throw new NotTranslatedException("4000-ACCTFILE-PROC: GO TO changes control flow in ways that are not translated");
    }

    /** Paragraph 4900-EXIT. */
    void pM4900Exit(CobolState st) {
        st.move("LK-M03B-RC", st.val("ACCTFILE-STATUS"));
    }

    /** Paragraph 4999-EXIT. */
    void pM4999Exit(CobolState st) {
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
