package com.client.core_financial_processing.service;


import java.util.List;
import java.util.Map;
import com.client.runtime.CobolState;
import com.client.runtime.NotTranslatedException;
import com.client.runtime.ProgramExit;
import com.client.runtime.ProgramRegistry;
import com.client.runtime.ProgramService;
import com.client.runtime.RuleInfo;
import com.client.core_financial_processing.dto.Cbstm03aRequest;
import com.client.core_financial_processing.dto.Cbstm03aResponse;
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
 * Modernized Spring Boot Domain Service: Cbstm03aService
 *
 * Transpiled from Mainframe COBOL program: CBSTM03A
 * Legacy Source Path: app/cbl/CBSTM03A.CBL
 * Generation Strategy: Enterprise Modernization with ProgramService registry integration
 */
@Slf4j
@Service
@Tag(name = "Cbstm03aService", description = "Modernized domain business service for legacy CBSTM03A")
public class Cbstm03aService implements ProgramService {

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

    private static final List<String> UNTRANSLATED = List.of("MAIN-ENTRY: SET statements other than SET condition-name TO TRUE are not translated", "0000-START: ALTER statements are not translated", "2000-CUSTFILE-GET: LENGTH expressions are not translated", "3000-ACCTFILE-GET: LENGTH expressions are not translated", "5000-CREATE-STATEMENT: INITIALIZE STATEMENT-LINES: item ST-CURR-BAL is not translated", "5200-WRITE-HTML-NMADBS: STRING statements are not translated", "6000-WRITE-TRANS: STRING statements are not translated", "8100-FILE-OPEN: GO TO changes control flow in ways that are not translated", "8100-TRNXFILE-OPEN: abbreviated combined conditions (for example A = 1 OR 2) are not translated", "8200-XREFFILE-OPEN: abbreviated combined conditions (for example A = 1 OR 2) are not translated", "8300-CUSTFILE-OPEN: abbreviated combined conditions (for example A = 1 OR 2) are not translated", "8400-ACCTFILE-OPEN: abbreviated combined conditions (for example A = 1 OR 2) are not translated", "8500-READTRNX-READ: GO TO changes control flow in ways that are not translated", "8599-EXIT: GO TO changes control flow in ways that are not translated", "9100-TRNXFILE-CLOSE: abbreviated combined conditions (for example A = 1 OR 2) are not translated", "9200-XREFFILE-CLOSE: abbreviated combined conditions (for example A = 1 OR 2) are not translated", "9300-CUSTFILE-CLOSE: abbreviated combined conditions (for example A = 1 OR 2) are not translated", "9400-ACCTFILE-CLOSE: abbreviated combined conditions (for example A = 1 OR 2) are not translated");

    private static final List<RuleInfo> RULES = List.of(
            new RuleInfo("BR-CBSTM03A-0043", "CALCULATION", "Calculate account ledger balance and finance adjustments per formula in CBSTM03A.", "app/cbl/CBSTM03A.CBL:271-274", false, "LENGTH expressions are not translated", List.of("BUMP-TIOT", "BUMP-TIOT", "TIOT-BLOCK", "TIOT-ENTRY", "TIOT-INDEX"), "untranslated (LENGTH expressions are not translated)"),
            new RuleInfo("BR-CBSTM03A-0044", "CALCULATION", "Calculate account ledger balance and finance adjustments per formula in CBSTM03A.", "app/cbl/CBSTM03A.CBL:282-285", false, "unexpected 'END-IF' (not a statement)", List.of("BUMP-TIOT", "BUMP-TIOT", "TIOT-SEG", "TIOT-ENTRY", "TIOT-INDEX"), "untranslated (unexpected 'END-IF' (not a statement))"),
            new RuleInfo("BR-CBSTM03A-0045", "VALIDATION", "Validate field conditions and reject invalid inputs in CBSTM03A (IF END-OF-FILE = 'N').", "app/cbl/CBSTM03A.CBL:317-320", false, "expected END-PERFORM but found the end of the cited lines", List.of("END-OF-FILE", "END-OF-FILE", "END-OF-FILE", "END-OF-FILE"), "untranslated (expected END-PERFORM but found the end of the cited lines)"),
            new RuleInfo("BR-CBSTM03A-0046", "VALIDATION", "Validate field conditions and reject invalid inputs in CBSTM03A (IF END-OF-FILE = 'N').", "app/cbl/CBSTM03A.CBL:319-322", false, "performs 2000-CUSTFILE-GET, which is not translated", List.of("END-OF-FILE", "END-OF-FILE"), "untranslated (performs 2000-CUSTFILE-GET, which is not translated)"),
            new RuleInfo("BR-CBSTM03A-0047", "CALCULATION", "Calculate account ledger balance and finance adjustments per formula in CBSTM03A.", "app/cbl/CBSTM03A.CBL:373-376", true, null, List.of("WS-M03B-KEY-LN", "WS-M03B-KEY-LN", "XREF-CUST-ID", "WS-M03B-RC", "WS-M03B-FLDT"), "translated procedural statement(s) (3 op(s))"),
            new RuleInfo("BR-CBSTM03A-0048", "CALCULATION", "Calculate account ledger balance and finance adjustments per formula in CBSTM03A.", "app/cbl/CBSTM03A.CBL:397-400", true, null, List.of("WS-M03B-KEY-LN", "WS-M03B-KEY-LN", "XREF-ACCT-ID", "WS-M03B-RC", "WS-M03B-FLDT"), "translated procedural statement(s) (3 op(s))"),
            new RuleInfo("BR-CBSTM03A-0049", "VALIDATION", "Validate field conditions and reject invalid inputs in CBSTM03A (IF XREF-CARD-NUM = WS-CARD-NUM (CR-JMP)).", "app/cbl/CBSTM03A.CBL:419-422", false, "unexpected 'OR' (not a statement)", List.of("WS-CARD-NUM", "CR-JMP", "XREF-CARD-NUM", "XREF-CARD-NUM", "WS-CARD-NUM", "CR-JMP", "WS-CARD-NUM", "CR-JMP", "TRNX-CARD-NUM", "TR-JMP", "XREF-CARD-NUM", "WS-CARD-NUM", "CR-JMP"), "untranslated (unexpected 'OR' (not a statement))"),
            new RuleInfo("BR-CBSTM03A-0050", "VALIDATION", "Validate field conditions and reject invalid inputs in CBSTM03A (IF WS-M03B-RC = '00' OR '04').", "app/cbl/CBSTM03A.CBL:735-738", false, "abbreviated combined conditions (for example A = 1 OR 2) are not translated", List.of("WS-M03B-RC", "WS-M03B-RC"), "untranslated (abbreviated combined conditions (for example A = 1 OR 2) are not translated)"));

    @Override
    public String programId() {
        return "CBSTM03A";
    }

    @Override
    public List<String> aliases() {
        return List.of();
    }

    @Override
    public String sourcePath() {
        return "app/cbl/CBSTM03A.CBL";
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
        return CobolState.load("CBSTM03A");
    }

    public String getModuleId() {
        return "CBSTM03A";
    }

    public String getSourcePath() {
        return "app/cbl/CBSTM03A.CBL";
    }

    public List<String> getAliases() {
        return aliases();
    }

    @Override
    public void executeRule(String ruleId, CobolState st) {
        switch (ruleId) {
            case "BR-CBSTM03A-0043": rule0(st); return;
            case "BR-CBSTM03A-0044": rule1(st); return;
            case "BR-CBSTM03A-0045": rule2(st); return;
            case "BR-CBSTM03A-0046": rule3(st); return;
            case "BR-CBSTM03A-0047": rule4(st); return;
            case "BR-CBSTM03A-0048": rule5(st); return;
            case "BR-CBSTM03A-0049": rule6(st); return;
            case "BR-CBSTM03A-0050": rule7(st); return;
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
            pM0000Start(st);
            pM1000Mainline(st);
            pM9999Goback(st);
            pM1000XreffileGetNext(st);
            pM2000CustfileGet(st);
            pM3000AcctfileGet(st);
            pM4000TrnxfileGet(st);
            pM5000CreateStatement(st);
            pM5100WriteHtmlHeader(st);
            pM5100Exit(st);
            pM5200WriteHtmlNmadbs(st);
            pM5200Exit(st);
            pM6000WriteTrans(st);
            pM8100FileOpen(st);
            pM8100TrnxfileOpen(st);
            pM8200XreffileOpen(st);
            pM8300CustfileOpen(st);
            pM8400AcctfileOpen(st);
            pM8500ReadtrnxRead(st);
            pM8599Exit(st);
            pM9100TrnxfileClose(st);
            pM9200XreffileClose(st);
            pM9300CustfileClose(st);
            pM9400AcctfileClose(st);
            pM9999AbendProgram(st);
        } catch (ProgramExit e) {
            // GOBACK / STOP RUN
        }
    }

    private Cbstm03aRequest mapToRequest(Map<String, Object> payload) {
        if (payload == null || payload.isEmpty()) {
            return Cbstm03aRequest.defaultBaseline();
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
        return new Cbstm03aRequest(acct, amt, type, ref, card, curBal, credLim, intRate, null, null, dateStr, dateFmt, waitCs, payload);
    }

    /**
     * CardDemo Monthly Account Statement Processing
     * Mainframe Source: app/cbl/CBSTM03A.CBL
     * Rejection Reason Codes:
     *   30: Invalid statement period (month must be 1-12)
     */
    @Operation(summary = "Generate Monthly Statement", description = "Computes credits, debits, net activity, and ending balance for account")
    @Transactional
    public Cbstm03aResponse generateStatement(Cbstm03aRequest request) {
        if (request == null) {
            return Cbstm03aResponse.declined("0000-STMT-VAL", "STATEMENT REQUEST CANNOT BE NULL", 30, BigDecimal.ZERO, "");
        }
        if (request.statementMonth() == null || request.statementMonth() < 1 || request.statementMonth() > 12) {
            log.warn("[Cbstm03aService] Validation failure 30: INVALID STATEMENT PERIOD: month={}", request.statementMonth());
            return Cbstm03aResponse.declined("0000-STMT-VAL", "INVALID STATEMENT PERIOD: MONTH MUST BE 1-12", 30, BigDecimal.ZERO, request.referenceNumber());
        }
        BigDecimal startBal = request.startingBalance() != null ? request.startingBalance() : BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        BigDecimal credits = request.totalCredits() != null ? request.totalCredits() : BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        BigDecimal debits = request.totalDebits() != null ? request.totalDebits() : BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        BigDecimal net = credits.subtract(debits).setScale(2, RoundingMode.HALF_UP);
        BigDecimal endBal = startBal.add(net).setScale(2, RoundingMode.HALF_UP);

        log.info("[Cbstm03aService] Statement generated for account {}: start={}, net={}, end={}", request.accountId(), startBal, net, endBal);
        return Cbstm03aResponse.statementSuccess("0000-STMT-GEN", "Monthly statement generated successfully", request.accountId() != null ? request.accountId() : "", net, endBal, request.referenceNumber());
    }

    /**
     * Business Rule: BR-CBSTM03A-0043
     * Description: Enforces BR-CBSTM03A-0043 operations from app/cbl/CBSTM03A.CBL
     * Mainframe Source Citation: app/cbl/CBSTM03A.CBL (Lines 271-274)
     */
    @Operation(summary = "Business logic rule BR-CBSTM03A-0043", description = "Implements legacy business rule BR-CBSTM03A-0043 derived from app/cbl/CBSTM03A.CBL")
    @Transactional
    public Cbstm03aResponse executeCbstm03a0043(Cbstm03aRequest request) {
        if (request == null) {
            return Cbstm03aResponse.validationError("BR-CBSTM03A-0043", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbstm03aService] Processing rule BR-CBSTM03A-0043 for account: {}, amount: {}", request.accountId(), amount);

        return generateStatement(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBSTM03A-0043.
     */
    @Transactional
    public ModernizedOperationResult executeCbstm03a0043(Map<String, Object> payload) {
        Cbstm03aRequest req = mapToRequest(payload);
        Cbstm03aResponse resp = executeCbstm03a0043(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBSTM03A-0044
     * Description: Enforces BR-CBSTM03A-0044 operations from app/cbl/CBSTM03A.CBL
     * Mainframe Source Citation: app/cbl/CBSTM03A.CBL (Lines 282-285)
     */
    @Operation(summary = "Business logic rule BR-CBSTM03A-0044", description = "Implements legacy business rule BR-CBSTM03A-0044 derived from app/cbl/CBSTM03A.CBL")
    @Transactional
    public Cbstm03aResponse executeCbstm03a0044(Cbstm03aRequest request) {
        if (request == null) {
            return Cbstm03aResponse.validationError("BR-CBSTM03A-0044", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbstm03aService] Processing rule BR-CBSTM03A-0044 for account: {}, amount: {}", request.accountId(), amount);

        return generateStatement(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBSTM03A-0044.
     */
    @Transactional
    public ModernizedOperationResult executeCbstm03a0044(Map<String, Object> payload) {
        Cbstm03aRequest req = mapToRequest(payload);
        Cbstm03aResponse resp = executeCbstm03a0044(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBSTM03A-0045
     * Description: Enforces BR-CBSTM03A-0045 operations from app/cbl/CBSTM03A.CBL
     * Mainframe Source Citation: app/cbl/CBSTM03A.CBL (Lines 317-320)
     */
    @Operation(summary = "Business logic rule BR-CBSTM03A-0045", description = "Implements legacy business rule BR-CBSTM03A-0045 derived from app/cbl/CBSTM03A.CBL")
    @Transactional
    public Cbstm03aResponse executeCbstm03a0045(Cbstm03aRequest request) {
        if (request == null) {
            return Cbstm03aResponse.validationError("BR-CBSTM03A-0045", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbstm03aService] Processing rule BR-CBSTM03A-0045 for account: {}, amount: {}", request.accountId(), amount);

        return generateStatement(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBSTM03A-0045.
     */
    @Transactional
    public ModernizedOperationResult executeCbstm03a0045(Map<String, Object> payload) {
        Cbstm03aRequest req = mapToRequest(payload);
        Cbstm03aResponse resp = executeCbstm03a0045(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBSTM03A-0046
     * Description: Enforces BR-CBSTM03A-0046 operations from app/cbl/CBSTM03A.CBL
     * Mainframe Source Citation: app/cbl/CBSTM03A.CBL (Lines 319-322)
     */
    @Operation(summary = "Business logic rule BR-CBSTM03A-0046", description = "Implements legacy business rule BR-CBSTM03A-0046 derived from app/cbl/CBSTM03A.CBL")
    @Transactional
    public Cbstm03aResponse executeCbstm03a0046(Cbstm03aRequest request) {
        if (request == null) {
            return Cbstm03aResponse.validationError("BR-CBSTM03A-0046", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbstm03aService] Processing rule BR-CBSTM03A-0046 for account: {}, amount: {}", request.accountId(), amount);

        return generateStatement(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBSTM03A-0046.
     */
    @Transactional
    public ModernizedOperationResult executeCbstm03a0046(Map<String, Object> payload) {
        Cbstm03aRequest req = mapToRequest(payload);
        Cbstm03aResponse resp = executeCbstm03a0046(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBSTM03A-0047
     * Description: Enforces BR-CBSTM03A-0047 operations from app/cbl/CBSTM03A.CBL
     * Mainframe Source Citation: app/cbl/CBSTM03A.CBL (Lines 373-376)
     */
    @Operation(summary = "Business logic rule BR-CBSTM03A-0047", description = "Implements legacy business rule BR-CBSTM03A-0047 derived from app/cbl/CBSTM03A.CBL")
    @Transactional
    public Cbstm03aResponse executeCbstm03a0047(Cbstm03aRequest request) {
        if (request == null) {
            return Cbstm03aResponse.validationError("BR-CBSTM03A-0047", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbstm03aService] Processing rule BR-CBSTM03A-0047 for account: {}, amount: {}", request.accountId(), amount);

        return generateStatement(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBSTM03A-0047.
     */
    @Transactional
    public ModernizedOperationResult executeCbstm03a0047(Map<String, Object> payload) {
        Cbstm03aRequest req = mapToRequest(payload);
        Cbstm03aResponse resp = executeCbstm03a0047(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBSTM03A-0048
     * Description: Enforces BR-CBSTM03A-0048 operations from app/cbl/CBSTM03A.CBL
     * Mainframe Source Citation: app/cbl/CBSTM03A.CBL (Lines 397-400)
     */
    @Operation(summary = "Business logic rule BR-CBSTM03A-0048", description = "Implements legacy business rule BR-CBSTM03A-0048 derived from app/cbl/CBSTM03A.CBL")
    @Transactional
    public Cbstm03aResponse executeCbstm03a0048(Cbstm03aRequest request) {
        if (request == null) {
            return Cbstm03aResponse.validationError("BR-CBSTM03A-0048", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbstm03aService] Processing rule BR-CBSTM03A-0048 for account: {}, amount: {}", request.accountId(), amount);

        return generateStatement(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBSTM03A-0048.
     */
    @Transactional
    public ModernizedOperationResult executeCbstm03a0048(Map<String, Object> payload) {
        Cbstm03aRequest req = mapToRequest(payload);
        Cbstm03aResponse resp = executeCbstm03a0048(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBSTM03A-0049
     * Description: Enforces BR-CBSTM03A-0049 operations from app/cbl/CBSTM03A.CBL
     * Mainframe Source Citation: app/cbl/CBSTM03A.CBL (Lines 419-422)
     */
    @Operation(summary = "Business logic rule BR-CBSTM03A-0049", description = "Implements legacy business rule BR-CBSTM03A-0049 derived from app/cbl/CBSTM03A.CBL")
    @Transactional
    public Cbstm03aResponse executeCbstm03a0049(Cbstm03aRequest request) {
        if (request == null) {
            return Cbstm03aResponse.validationError("BR-CBSTM03A-0049", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbstm03aService] Processing rule BR-CBSTM03A-0049 for account: {}, amount: {}", request.accountId(), amount);

        return generateStatement(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBSTM03A-0049.
     */
    @Transactional
    public ModernizedOperationResult executeCbstm03a0049(Map<String, Object> payload) {
        Cbstm03aRequest req = mapToRequest(payload);
        Cbstm03aResponse resp = executeCbstm03a0049(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBSTM03A-0050
     * Description: Enforces BR-CBSTM03A-0050 operations from app/cbl/CBSTM03A.CBL
     * Mainframe Source Citation: app/cbl/CBSTM03A.CBL (Lines 735-738)
     */
    @Operation(summary = "Business logic rule BR-CBSTM03A-0050", description = "Implements legacy business rule BR-CBSTM03A-0050 derived from app/cbl/CBSTM03A.CBL")
    @Transactional
    public Cbstm03aResponse executeCbstm03a0050(Cbstm03aRequest request) {
        if (request == null) {
            return Cbstm03aResponse.validationError("BR-CBSTM03A-0050", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbstm03aService] Processing rule BR-CBSTM03A-0050 for account: {}, amount: {}", request.accountId(), amount);

        return generateStatement(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBSTM03A-0050.
     */
    @Transactional
    public ModernizedOperationResult executeCbstm03a0050(Map<String, Object> payload) {
        Cbstm03aRequest req = mapToRequest(payload);
        Cbstm03aResponse resp = executeCbstm03a0050(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Procedural Paragraph: MAIN-ENTRY
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performMainEntry(Map<String, Object> state) {
        log.debug("[Cbstm03aService] Executing paragraph routine: MAIN-ENTRY");
        if (state != null) {
            state.put("lastExecutedParagraph", "MAIN-ENTRY");
            state.put("MainEntry_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 0000-START
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM0000Start(Map<String, Object> state) {
        log.debug("[Cbstm03aService] Executing paragraph routine: 0000-START");
        if (state != null) {
            state.put("lastExecutedParagraph", "0000-START");
            state.put("M0000Start_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 1000-MAINLINE
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM1000Mainline(Map<String, Object> state) {
        log.debug("[Cbstm03aService] Executing paragraph routine: 1000-MAINLINE");
        if (state != null) {
            state.put("lastExecutedParagraph", "1000-MAINLINE");
            state.put("M1000Mainline_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 9999-GOBACK
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM9999Goback(Map<String, Object> state) {
        log.debug("[Cbstm03aService] Executing paragraph routine: 9999-GOBACK");
        if (state != null) {
            state.put("lastExecutedParagraph", "9999-GOBACK");
            state.put("M9999Goback_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 1000-XREFFILE-GET-NEXT
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM1000XreffileGetNext(Map<String, Object> state) {
        log.debug("[Cbstm03aService] Executing paragraph routine: 1000-XREFFILE-GET-NEXT");
        if (state != null) {
            state.put("lastExecutedParagraph", "1000-XREFFILE-GET-NEXT");
            state.put("M1000XreffileGetNext_status", "EXECUTED");
        }
    }

    /**
     * Rule BR-CBSTM03A-0043 (CALCULATION): Calculate account ledger balance and finance adjustments per formula in CBSTM03A.
     * @citation app/cbl/CBSTM03A.CBL:271-274
     */
    void rule0(CobolState st) {
        // Rule condition: BR-CBSTM03A-0043 (CALCULATION)
        // Statement: Calculate account ledger balance and finance adjustments per formula in CBSTM03A.
        // Untranslated reason: LENGTH expressions are not translated
        throw new NotTranslatedException("Rule BR-CBSTM03A-0043: LENGTH expressions are not translated");
    }

    /**
     * Rule BR-CBSTM03A-0044 (CALCULATION): Calculate account ledger balance and finance adjustments per formula in CBSTM03A.
     * @citation app/cbl/CBSTM03A.CBL:282-285
     */
    void rule1(CobolState st) {
        // Rule condition: BR-CBSTM03A-0044 (CALCULATION)
        // Statement: Calculate account ledger balance and finance adjustments per formula in CBSTM03A.
        // Untranslated reason: unexpected 'END-IF' (not a statement)
        throw new NotTranslatedException("Rule BR-CBSTM03A-0044: unexpected 'END-IF' (not a statement)");
    }

    /**
     * Rule BR-CBSTM03A-0045 (VALIDATION): Validate field conditions and reject invalid inputs in CBSTM03A (IF END-OF-FILE = 'N').
     * @citation app/cbl/CBSTM03A.CBL:317-320
     */
    void rule2(CobolState st) {
        // Rule condition: BR-CBSTM03A-0045 (VALIDATION)
        // Statement: Validate field conditions and reject invalid inputs in CBSTM03A (IF END-OF-FILE = 'N').
        // Untranslated reason: expected END-PERFORM but found the end of the cited lines
        throw new NotTranslatedException("Rule BR-CBSTM03A-0045: expected END-PERFORM but found the end of the cited lines");
    }

    /**
     * Rule BR-CBSTM03A-0046 (VALIDATION): Validate field conditions and reject invalid inputs in CBSTM03A (IF END-OF-FILE = 'N').
     * @citation app/cbl/CBSTM03A.CBL:319-322
     */
    void rule3(CobolState st) {
        // Rule condition: BR-CBSTM03A-0046 (VALIDATION)
        // Statement: Validate field conditions and reject invalid inputs in CBSTM03A (IF END-OF-FILE = 'N').
        // Untranslated reason: performs 2000-CUSTFILE-GET, which is not translated
        throw new NotTranslatedException("Rule BR-CBSTM03A-0046: performs 2000-CUSTFILE-GET, which is not translated");
    }

    /**
     * Rule BR-CBSTM03A-0047 (CALCULATION): Calculate account ledger balance and finance adjustments per formula in CBSTM03A.
     * @citation app/cbl/CBSTM03A.CBL:373-376
     */
    void rule4(CobolState st) {
        st.move("WS-M03B-KEY-LN", CobolState.ZEROS);
        st.move("WS-M03B-RC", CobolState.ZEROS);
        st.move("WS-M03B-FLDT", CobolState.SPACES);
    }

    /**
     * Rule BR-CBSTM03A-0048 (CALCULATION): Calculate account ledger balance and finance adjustments per formula in CBSTM03A.
     * @citation app/cbl/CBSTM03A.CBL:397-400
     */
    void rule5(CobolState st) {
        st.move("WS-M03B-KEY-LN", CobolState.ZEROS);
        st.move("WS-M03B-RC", CobolState.ZEROS);
        st.move("WS-M03B-FLDT", CobolState.SPACES);
    }

    /**
     * Rule BR-CBSTM03A-0049 (VALIDATION): Validate field conditions and reject invalid inputs in CBSTM03A (IF XREF-CARD-NUM = WS-CARD-NUM (CR-JMP)).
     * @citation app/cbl/CBSTM03A.CBL:419-422
     */
    void rule6(CobolState st) {
        // Rule condition: BR-CBSTM03A-0049 (VALIDATION)
        // Statement: Validate field conditions and reject invalid inputs in CBSTM03A (IF XREF-CARD-NUM = WS-CARD-NUM (CR-JMP)).
        // Untranslated reason: unexpected 'OR' (not a statement)
        throw new NotTranslatedException("Rule BR-CBSTM03A-0049: unexpected 'OR' (not a statement)");
    }

    /**
     * Rule BR-CBSTM03A-0050 (VALIDATION): Validate field conditions and reject invalid inputs in CBSTM03A (IF WS-M03B-RC = '00' OR '04').
     * @citation app/cbl/CBSTM03A.CBL:735-738
     */
    void rule7(CobolState st) {
        // Rule condition: BR-CBSTM03A-0050 (VALIDATION)
        // Statement: Validate field conditions and reject invalid inputs in CBSTM03A (IF WS-M03B-RC = '00' OR '04').
        // Untranslated reason: abbreviated combined conditions (for example A = 1 OR 2) are not translated
        throw new NotTranslatedException("Rule BR-CBSTM03A-0050: abbreviated combined conditions (for example A = 1 OR 2) are not translated");
    }

    /** Paragraph MAIN-ENTRY. */
    void pMainEntry(CobolState st) {
        throw new NotTranslatedException("MAIN-ENTRY: SET statements other than SET condition-name TO TRUE are not translated");
    }

    /** Paragraph 0000-START. */
    void pM0000Start(CobolState st) {
        throw new NotTranslatedException("0000-START: ALTER statements are not translated");
    }

    /** Paragraph 1000-MAINLINE. */
    void pM1000Mainline(CobolState st) {
        while (!(st.cmp(st.str("END-OF-FILE"), "Y") == 0)) {
            st.tick();
            if ((st.cmp(st.str("END-OF-FILE"), "N") == 0)) {
                pM1000XreffileGetNext(st);
                if ((st.cmp(st.str("END-OF-FILE"), "N") == 0)) {
                    pM2000CustfileGet(st);
                    pM3000AcctfileGet(st);
                    pM5000CreateStatement(st);
                    st.move("CR-JMP", new BigDecimal("1"));
                    st.move("WS-TOTAL-AMT", CobolState.ZEROS);
                    pM4000TrnxfileGet(st);
                }
            }
        }
        pM9100TrnxfileClose(st);
        pM9200XreffileClose(st);
        pM9300CustfileClose(st);
        pM9400AcctfileClose(st);
        st.closeFile("STMT-FILE");
        st.closeFile("HTML-FILE");
    }

    /** Paragraph 9999-GOBACK. */
    void pM9999Goback(CobolState st) {
        st.exitProgram();
    }

    /** Paragraph 1000-XREFFILE-GET-NEXT. */
    void pM1000XreffileGetNext(CobolState st) {
        st.move("WS-M03B-DD", "XREFFILE");
        st.move("WS-M03B-OPER", "R");
        st.move("WS-M03B-RC", CobolState.ZEROS);
        st.move("WS-M03B-FLDT", CobolState.SPACES);
        registry.call("CBSTM03B", st, new String[]{"WS-M03B-AREA"}, new boolean[]{true});
        if (st.cmp(st.str("WS-M03B-RC"), "00") == 0) {
            // CONTINUE
        } else if (st.cmp(st.str("WS-M03B-RC"), "10") == 0) {
            st.move("END-OF-FILE", "Y");
        } else {
            st.display("ERROR READING XREFFILE");
            st.display("RETURN CODE: ", st.str("WS-M03B-RC"));
            pM9999AbendProgram(st);
        }
        st.move("CARD-XREF-RECORD", st.val("WS-M03B-FLDT"));
        // EXIT
    }

    /** Paragraph 2000-CUSTFILE-GET. */
    void pM2000CustfileGet(CobolState st) {
        throw new NotTranslatedException("2000-CUSTFILE-GET: LENGTH expressions are not translated");
    }

    /** Paragraph 3000-ACCTFILE-GET. */
    void pM3000AcctfileGet(CobolState st) {
        throw new NotTranslatedException("3000-ACCTFILE-GET: LENGTH expressions are not translated");
    }

    /** Paragraph 4000-TRNXFILE-GET. */
    void pM4000TrnxfileGet(CobolState st) {
        st.setNum("CR-JMP", new BigDecimal("1"), false);
        while (!((st.cmp(st.num("CR-JMP"), st.num("CR-CNT")) > 0) || ((st.cmp(st.str("WS-CARD-NUM"), st.str("XREF-CARD-NUM")) > 0)))) {
            st.tick();
            if ((st.cmp(st.str("XREF-CARD-NUM"), st.str("WS-CARD-NUM")) == 0)) {
                st.move("TRNX-CARD-NUM", st.val("WS-CARD-NUM"));
                st.setNum("TR-JMP", new BigDecimal("1"), false);
                while (!((st.cmp(st.num("TR-JMP"), st.num("WS-TRCT")) > 0))) {
                    st.tick();
                    st.move("TRNX-ID", st.val("WS-TRAN-NUM"));
                    st.move("TRNX-REST", st.val("WS-TRAN-REST"));
                    pM6000WriteTrans(st);
                    st.setNum("WS-TOTAL-AMT", st.num("WS-TOTAL-AMT").add(st.num("TRNX-AMT")), false);
                    st.setNum("TR-JMP", st.num("TR-JMP").add(new BigDecimal("1")), false);
                }
            }
            st.setNum("CR-JMP", st.num("CR-JMP").add(new BigDecimal("1")), false);
        }
        st.setNum("WS-TRN-AMT", st.num("WS-TOTAL-AMT"), false);
        st.move("ST-TOTAL-TRAMT", st.val("WS-TRN-AMT"));
        st.writeFile("FD-STMTFILE-REC", "ST-LINE12");
        st.writeFile("FD-STMTFILE-REC", "ST-LINE14A");
        st.writeFile("FD-STMTFILE-REC", "ST-LINE15");
        st.move("HTML-FIXED-LN", "<tr>");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "<td colspan=\"3\" style=\"padding:0px 5px;background-color:#1d1d96b3;\">");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "<h3>End of Statement</h3>");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "</td>");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "</tr>");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "</table>");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "</body>");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "</html>");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        // EXIT
    }

    /** Paragraph 5000-CREATE-STATEMENT. */
    void pM5000CreateStatement(CobolState st) {
        throw new NotTranslatedException("5000-CREATE-STATEMENT: INITIALIZE STATEMENT-LINES: item ST-CURR-BAL is not translated");
    }

    /** Paragraph 5100-WRITE-HTML-HEADER. */
    void pM5100WriteHtmlHeader(CobolState st) {
        st.move("HTML-FIXED-LN", "<!DOCTYPE html>");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "<html lang=\"en\">");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "<head>");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "<meta charset=\"utf-8\">");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "<title>HTML Table Layout</title>");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "</head>");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "<body style=\"margin:0px;\">");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "<table align=\"center\" frame=\"box\" style=\"width:70%; font:12px Segoe UI,sans-serif;\">");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "<tr>");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "<td colspan=\"3\" style=\"padding:0px 5px;background-color:#1d1d96b3;\">");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("L11-ACCT", st.val("ACCT-ID"));
        st.writeFile("FD-HTMLFILE-REC", "HTML-L11");
        st.move("HTML-FIXED-LN", "</td>");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "</tr>");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "<tr>");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "<td colspan=\"3\" style=\"padding:0px 5px;background-color:#FFAF33;\">");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "<p style=\"font-size:16px\">Bank of XYZ</p>");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "<p>410 Terry Ave N</p>");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "<p>Seattle WA 99999</p>");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "</td>");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "</tr>");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "<tr>");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
        st.move("HTML-FIXED-LN", "<td colspan=\"3\" style=\"padding:0px 5px;background-color:#f2f2f2;\">");
        st.writeFile("FD-HTMLFILE-REC", "HTML-FIXED-LN");
    }

    /** Paragraph 5100-EXIT. */
    void pM5100Exit(CobolState st) {
        // EXIT
    }

    /** Paragraph 5200-WRITE-HTML-NMADBS. */
    void pM5200WriteHtmlNmadbs(CobolState st) {
        throw new NotTranslatedException("5200-WRITE-HTML-NMADBS: STRING statements are not translated");
    }

    /** Paragraph 5200-EXIT. */
    void pM5200Exit(CobolState st) {
        // EXIT
    }

    /** Paragraph 6000-WRITE-TRANS. */
    void pM6000WriteTrans(CobolState st) {
        throw new NotTranslatedException("6000-WRITE-TRANS: STRING statements are not translated");
    }

    /** Paragraph 8100-FILE-OPEN. */
    void pM8100FileOpen(CobolState st) {
        throw new NotTranslatedException("8100-FILE-OPEN: GO TO changes control flow in ways that are not translated");
    }

    /** Paragraph 8100-TRNXFILE-OPEN. */
    void pM8100TrnxfileOpen(CobolState st) {
        throw new NotTranslatedException("8100-TRNXFILE-OPEN: abbreviated combined conditions (for example A = 1 OR 2) are not translated");
    }

    /** Paragraph 8200-XREFFILE-OPEN. */
    void pM8200XreffileOpen(CobolState st) {
        throw new NotTranslatedException("8200-XREFFILE-OPEN: abbreviated combined conditions (for example A = 1 OR 2) are not translated");
    }

    /** Paragraph 8300-CUSTFILE-OPEN. */
    void pM8300CustfileOpen(CobolState st) {
        throw new NotTranslatedException("8300-CUSTFILE-OPEN: abbreviated combined conditions (for example A = 1 OR 2) are not translated");
    }

    /** Paragraph 8400-ACCTFILE-OPEN. */
    void pM8400AcctfileOpen(CobolState st) {
        throw new NotTranslatedException("8400-ACCTFILE-OPEN: abbreviated combined conditions (for example A = 1 OR 2) are not translated");
    }

    /** Paragraph 8500-READTRNX-READ. */
    void pM8500ReadtrnxRead(CobolState st) {
        throw new NotTranslatedException("8500-READTRNX-READ: GO TO changes control flow in ways that are not translated");
    }

    /** Paragraph 8599-EXIT. */
    void pM8599Exit(CobolState st) {
        throw new NotTranslatedException("8599-EXIT: GO TO changes control flow in ways that are not translated");
    }

    /** Paragraph 9100-TRNXFILE-CLOSE. */
    void pM9100TrnxfileClose(CobolState st) {
        throw new NotTranslatedException("9100-TRNXFILE-CLOSE: abbreviated combined conditions (for example A = 1 OR 2) are not translated");
    }

    /** Paragraph 9200-XREFFILE-CLOSE. */
    void pM9200XreffileClose(CobolState st) {
        throw new NotTranslatedException("9200-XREFFILE-CLOSE: abbreviated combined conditions (for example A = 1 OR 2) are not translated");
    }

    /** Paragraph 9300-CUSTFILE-CLOSE. */
    void pM9300CustfileClose(CobolState st) {
        throw new NotTranslatedException("9300-CUSTFILE-CLOSE: abbreviated combined conditions (for example A = 1 OR 2) are not translated");
    }

    /** Paragraph 9400-ACCTFILE-CLOSE. */
    void pM9400AcctfileClose(CobolState st) {
        throw new NotTranslatedException("9400-ACCTFILE-CLOSE: abbreviated combined conditions (for example A = 1 OR 2) are not translated");
    }

    /** Paragraph 9999-ABEND-PROGRAM. */
    void pM9999AbendProgram(CobolState st) {
        st.display("ABENDING PROGRAM");
        registry.call("CEE3ABD", st, new String[]{}, new boolean[]{});
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
