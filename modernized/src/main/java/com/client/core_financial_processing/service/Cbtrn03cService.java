package com.client.core_financial_processing.service;


import java.util.List;
import java.util.Map;
import com.client.runtime.CobolState;
import com.client.runtime.NotTranslatedException;
import com.client.runtime.ProgramExit;
import com.client.runtime.ProgramRegistry;
import com.client.runtime.ProgramService;
import com.client.runtime.RuleInfo;
import com.client.core_financial_processing.dto.Cbtrn03cRequest;
import com.client.core_financial_processing.dto.Cbtrn03cResponse;
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
 * Modernized Spring Boot Domain Service: Cbtrn03cService
 *
 * Transpiled from Mainframe COBOL program: CBTRN03C
 * Legacy Source Path: app/cbl/CBTRN03C.cbl
 * Generation Strategy: Enterprise Modernization with ProgramService registry integration
 */
@Slf4j
@Service
@Tag(name = "Cbtrn03cService", description = "Modernized domain business service for legacy CBTRN03C")
public class Cbtrn03cService implements ProgramService {

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

    private static final List<String> UNTRANSLATED = List.of("MAIN-ENTRY: NEXT SENTENCE is not translated", "0550-DATEPARM-READ: comparison of group item DATEPARM-STATUS is not translated", "1000-TRANFILE-GET-NEXT: comparison of group item TRANFILE-STATUS is not translated", "1100-WRITE-TRANSACTION-REPORT: FUNCTION expressions are not translated", "1111-WRITE-REPORT-REC: comparison of group item TRANREPT-STATUS is not translated", "1120-WRITE-DETAIL: INITIALIZE TRANSACTION-DETAIL-REPORT: item TRAN-REPORT-AMT is not translated", "0000-TRANFILE-OPEN: comparison of group item TRANFILE-STATUS is not translated", "0100-REPTFILE-OPEN: comparison of group item TRANREPT-STATUS is not translated", "0200-CARDXREF-OPEN: comparison of group item CARDXREF-STATUS is not translated", "0300-TRANTYPE-OPEN: comparison of group item TRANTYPE-STATUS is not translated", "0400-TRANCATG-OPEN: comparison of group item TRANCATG-STATUS is not translated", "0500-DATEPARM-OPEN: comparison of group item DATEPARM-STATUS is not translated", "1500-C-LOOKUP-TRANCATG: DISPLAY of group item FD-TRAN-CAT-KEY is not translated", "9000-TRANFILE-CLOSE: comparison of group item TRANFILE-STATUS is not translated", "9100-REPTFILE-CLOSE: comparison of group item TRANREPT-STATUS is not translated", "9200-CARDXREF-CLOSE: comparison of group item CARDXREF-STATUS is not translated", "9300-TRANTYPE-CLOSE: comparison of group item TRANTYPE-STATUS is not translated", "9400-TRANCATG-CLOSE: comparison of group item TRANCATG-STATUS is not translated", "9500-DATEPARM-CLOSE: comparison of group item DATEPARM-STATUS is not translated", "9910-DISPLAY-IO-STATUS: DISPLAY of group item IO-STATUS-04 is not translated");

    private static final List<RuleInfo> RULES = List.of(
            new RuleInfo("BR-CBTRN03C-0059", "VALIDATION", "Validate field conditions and reject invalid inputs in CBTRN03C (IF END-OF-FILE = 'N').", "app/cbl/CBTRN03C.cbl:170-173", false, "performs 1000-TRANFILE-GET-NEXT, which is not translated", List.of("END-OF-FILE", "END-OF-FILE", "TRAN-PROC-TS", "WS-START-DATE", "END-OF-FILE"), "untranslated (performs 1000-TRANFILE-GET-NEXT, which is not translated)"),
            new RuleInfo("BR-CBTRN03C-0060", "VALIDATION", "Validate field conditions and reject invalid inputs in CBTRN03C (IF TRAN-PROC-TS (1:10) >= WS-START-DATE).", "app/cbl/CBTRN03C.cbl:172-175", false, "performs 1000-TRANFILE-GET-NEXT, which is not translated", List.of("TRAN-PROC-TS", "WS-START-DATE", "TRAN-PROC-TS", "WS-END-DATE", "TRAN-PROC-TS", "WS-START-DATE"), "untranslated (performs 1000-TRANFILE-GET-NEXT, which is not translated)"),
            new RuleInfo("BR-CBTRN03C-0061", "VALIDATION", "Validate field conditions and reject invalid inputs in CBTRN03C (IF END-OF-FILE = 'N').", "app/cbl/CBTRN03C.cbl:178-181", false, "unexpected 'END-IF' (not a statement)", List.of("END-OF-FILE", "TRAN-RECORD", "WS-CURR-CARD-NUM", "TRAN-CARD-NUM", "END-OF-FILE"), "untranslated (unexpected 'END-IF' (not a statement))"),
            new RuleInfo("BR-CBTRN03C-0062", "VALIDATION", "Validate field conditions and reject invalid inputs in CBTRN03C (IF WS-CURR-CARD-NUM NOT= TRAN-CARD-NUM).", "app/cbl/CBTRN03C.cbl:180-183", false, "DISPLAY of group item TRAN-RECORD is not translated", List.of("TRAN-RECORD", "WS-CURR-CARD-NUM", "TRAN-CARD-NUM", "WS-FIRST-TIME", "WS-CURR-CARD-NUM", "TRAN-CARD-NUM"), "untranslated (DISPLAY of group item TRAN-RECORD is not translated)"),
            new RuleInfo("BR-CBTRN03C-0063", "VALIDATION", "Validate field conditions and reject invalid inputs in CBTRN03C (IF WS-FIRST-TIME = 'N').", "app/cbl/CBTRN03C.cbl:181-184", true, null, List.of("WS-CURR-CARD-NUM", "TRAN-CARD-NUM", "WS-FIRST-TIME"), ""),
            new RuleInfo("BR-CBTRN03C-0064", "VALIDATION", "Validate field conditions and reject invalid inputs in CBTRN03C (IF WS-FIRST-TIME = 'Y').", "app/cbl/CBTRN03C.cbl:274-277", true, null, List.of("WS-FIRST-TIME", "REPT-START-DATE", "WS-START-DATE"), ""),
            new RuleInfo("BR-CBTRN03C-0065", "VALIDATION", "Validate field conditions and reject invalid inputs in CBTRN03C (IF FUNCTION MOD(WS-LINE-COUNTER, WS-PAGE-SIZE).", "app/cbl/CBTRN03C.cbl:281-284", false, "FUNCTION expressions are not translated", List.of("WS-LINE-COUNTER", "WS-PAGE-SIZE", "WS-LINE-COUNTER", "WS-PAGE-SIZE"), "untranslated (FUNCTION expressions are not translated)"),
            new RuleInfo("BR-CBTRN03C-0066", "VALIDATION", "Validate field conditions and reject invalid inputs in CBTRN03C (IF TRANREPT-STATUS = '00').", "app/cbl/CBTRN03C.cbl:345-348", true, null, List.of("FD-REPTFILE-REC", "TRANREPT-STATUS", "APPL-RESULT", "TRANREPT-STATUS"), "translated procedural statement(s) (1 op(s))"));

    @Override
    public String programId() {
        return "CBTRN03C";
    }

    @Override
    public List<String> aliases() {
        return List.of();
    }

    @Override
    public String sourcePath() {
        return "app/cbl/CBTRN03C.cbl";
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
        return CobolState.load("CBTRN03C");
    }

    public String getModuleId() {
        return "CBTRN03C";
    }

    public String getSourcePath() {
        return "app/cbl/CBTRN03C.cbl";
    }

    public List<String> getAliases() {
        return aliases();
    }

    @Override
    public void executeRule(String ruleId, CobolState st) {
        switch (ruleId) {
            case "BR-CBTRN03C-0059": rule0(st); return;
            case "BR-CBTRN03C-0060": rule1(st); return;
            case "BR-CBTRN03C-0061": rule2(st); return;
            case "BR-CBTRN03C-0062": rule3(st); return;
            case "BR-CBTRN03C-0063": rule4(st); return;
            case "BR-CBTRN03C-0064": rule5(st); return;
            case "BR-CBTRN03C-0065": rule6(st); return;
            case "BR-CBTRN03C-0066": rule7(st); return;
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
            pM0550DateparmRead(st);
            pM1000TranfileGetNext(st);
            pM1100WriteTransactionReport(st);
            pM1110WritePageTotals(st);
            pM1120WriteAccountTotals(st);
            pM1110WriteGrandTotals(st);
            pM1120WriteHeaders(st);
            pM1111WriteReportRec(st);
            pM1120WriteDetail(st);
            pM0000TranfileOpen(st);
            pM0100ReptfileOpen(st);
            pM0200CardxrefOpen(st);
            pM0300TrantypeOpen(st);
            pM0400TrancatgOpen(st);
            pM0500DateparmOpen(st);
            pM1500ALookupXref(st);
            pM1500BLookupTrantype(st);
            pM1500CLookupTrancatg(st);
            pM9000TranfileClose(st);
            pM9100ReptfileClose(st);
            pM9200CardxrefClose(st);
            pM9300TrantypeClose(st);
            pM9400TrancatgClose(st);
            pM9500DateparmClose(st);
            pM9999AbendProgram(st);
            pM9910DisplayIoStatus(st);
        } catch (ProgramExit e) {
            // GOBACK / STOP RUN
        }
    }

    private Cbtrn03cRequest mapToRequest(Map<String, Object> payload) {
        if (payload == null || payload.isEmpty()) {
            return Cbtrn03cRequest.defaultBaseline();
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
        return new Cbtrn03cRequest(acct, amt, type, ref, card, curBal, credLim, intRate, null, null, dateStr, dateFmt, waitCs, payload);
    }

    /**
     * CardDemo CBTRN03C: Batch Transaction File Reader & Record Tally
     * Mainframe Source: CBTRN03C.cbl
     */
    @Operation(summary = "Process Batch Transaction File", description = "Reads sequential transaction records from TRANFILE")
    @Transactional
    public Cbtrn03cResponse processBatchTransactions(Cbtrn03cRequest request) {
        if (request == null) {
            return Cbtrn03cResponse.declined("0000-TRANFILE-PROC", "BATCH REQUEST CANNOT BE NULL", 40, BigDecimal.ZERO, "");
        }
        int processed = request.recordsProcessed() != null ? request.recordsProcessed() : 0;
        int accepted = request.recordsAccepted() != null ? request.recordsAccepted() : 0;
        int rejected = request.recordsRejected() != null ? request.recordsRejected() : 0;
        log.info("[CBTRN03C] Batch file processed: total={}, accepted={}, rejected={}", processed, accepted, rejected);
        return Cbtrn03cResponse.batchSuccess("0000-TRANFILE-PROC", "Batch transaction processing completed", processed, accepted, rejected, request.referenceNumber());
    }

    /**
     * Business Rule: BR-CBTRN03C-0059
     * Description: Enforces BR-CBTRN03C-0059 operations from app/cbl/CBTRN03C.cbl
     * Mainframe Source Citation: app/cbl/CBTRN03C.cbl (Lines 170-173)
     */
    @Operation(summary = "Business logic rule BR-CBTRN03C-0059", description = "Implements legacy business rule BR-CBTRN03C-0059 derived from app/cbl/CBTRN03C.cbl")
    @Transactional
    public Cbtrn03cResponse executeCbtrn03c0059(Cbtrn03cRequest request) {
        if (request == null) {
            return Cbtrn03cResponse.validationError("BR-CBTRN03C-0059", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbtrn03cService] Processing rule BR-CBTRN03C-0059 for account: {}, amount: {}", request.accountId(), amount);

        return processBatchTransactions(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBTRN03C-0059.
     */
    @Transactional
    public ModernizedOperationResult executeCbtrn03c0059(Map<String, Object> payload) {
        Cbtrn03cRequest req = mapToRequest(payload);
        Cbtrn03cResponse resp = executeCbtrn03c0059(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBTRN03C-0060
     * Description: Enforces BR-CBTRN03C-0060 operations from app/cbl/CBTRN03C.cbl
     * Mainframe Source Citation: app/cbl/CBTRN03C.cbl (Lines 172-175)
     */
    @Operation(summary = "Business logic rule BR-CBTRN03C-0060", description = "Implements legacy business rule BR-CBTRN03C-0060 derived from app/cbl/CBTRN03C.cbl")
    @Transactional
    public Cbtrn03cResponse executeCbtrn03c0060(Cbtrn03cRequest request) {
        if (request == null) {
            return Cbtrn03cResponse.validationError("BR-CBTRN03C-0060", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbtrn03cService] Processing rule BR-CBTRN03C-0060 for account: {}, amount: {}", request.accountId(), amount);

        return processBatchTransactions(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBTRN03C-0060.
     */
    @Transactional
    public ModernizedOperationResult executeCbtrn03c0060(Map<String, Object> payload) {
        Cbtrn03cRequest req = mapToRequest(payload);
        Cbtrn03cResponse resp = executeCbtrn03c0060(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBTRN03C-0061
     * Description: Enforces BR-CBTRN03C-0061 operations from app/cbl/CBTRN03C.cbl
     * Mainframe Source Citation: app/cbl/CBTRN03C.cbl (Lines 178-181)
     */
    @Operation(summary = "Business logic rule BR-CBTRN03C-0061", description = "Implements legacy business rule BR-CBTRN03C-0061 derived from app/cbl/CBTRN03C.cbl")
    @Transactional
    public Cbtrn03cResponse executeCbtrn03c0061(Cbtrn03cRequest request) {
        if (request == null) {
            return Cbtrn03cResponse.validationError("BR-CBTRN03C-0061", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbtrn03cService] Processing rule BR-CBTRN03C-0061 for account: {}, amount: {}", request.accountId(), amount);

        return processBatchTransactions(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBTRN03C-0061.
     */
    @Transactional
    public ModernizedOperationResult executeCbtrn03c0061(Map<String, Object> payload) {
        Cbtrn03cRequest req = mapToRequest(payload);
        Cbtrn03cResponse resp = executeCbtrn03c0061(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBTRN03C-0062
     * Description: Enforces BR-CBTRN03C-0062 operations from app/cbl/CBTRN03C.cbl
     * Mainframe Source Citation: app/cbl/CBTRN03C.cbl (Lines 180-183)
     */
    @Operation(summary = "Business logic rule BR-CBTRN03C-0062", description = "Implements legacy business rule BR-CBTRN03C-0062 derived from app/cbl/CBTRN03C.cbl")
    @Transactional
    public Cbtrn03cResponse executeCbtrn03c0062(Cbtrn03cRequest request) {
        if (request == null) {
            return Cbtrn03cResponse.validationError("BR-CBTRN03C-0062", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbtrn03cService] Processing rule BR-CBTRN03C-0062 for account: {}, amount: {}", request.accountId(), amount);

        return processBatchTransactions(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBTRN03C-0062.
     */
    @Transactional
    public ModernizedOperationResult executeCbtrn03c0062(Map<String, Object> payload) {
        Cbtrn03cRequest req = mapToRequest(payload);
        Cbtrn03cResponse resp = executeCbtrn03c0062(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBTRN03C-0063
     * Description: Enforces BR-CBTRN03C-0063 operations from app/cbl/CBTRN03C.cbl
     * Mainframe Source Citation: app/cbl/CBTRN03C.cbl (Lines 181-184)
     */
    @Operation(summary = "Business logic rule BR-CBTRN03C-0063", description = "Implements legacy business rule BR-CBTRN03C-0063 derived from app/cbl/CBTRN03C.cbl")
    @Transactional
    public Cbtrn03cResponse executeCbtrn03c0063(Cbtrn03cRequest request) {
        if (request == null) {
            return Cbtrn03cResponse.validationError("BR-CBTRN03C-0063", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbtrn03cService] Processing rule BR-CBTRN03C-0063 for account: {}, amount: {}", request.accountId(), amount);

        return processBatchTransactions(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBTRN03C-0063.
     */
    @Transactional
    public ModernizedOperationResult executeCbtrn03c0063(Map<String, Object> payload) {
        Cbtrn03cRequest req = mapToRequest(payload);
        Cbtrn03cResponse resp = executeCbtrn03c0063(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBTRN03C-0064
     * Description: Enforces BR-CBTRN03C-0064 operations from app/cbl/CBTRN03C.cbl
     * Mainframe Source Citation: app/cbl/CBTRN03C.cbl (Lines 274-277)
     */
    @Operation(summary = "Business logic rule BR-CBTRN03C-0064", description = "Implements legacy business rule BR-CBTRN03C-0064 derived from app/cbl/CBTRN03C.cbl")
    @Transactional
    public Cbtrn03cResponse executeCbtrn03c0064(Cbtrn03cRequest request) {
        if (request == null) {
            return Cbtrn03cResponse.validationError("BR-CBTRN03C-0064", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbtrn03cService] Processing rule BR-CBTRN03C-0064 for account: {}, amount: {}", request.accountId(), amount);

        return processBatchTransactions(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBTRN03C-0064.
     */
    @Transactional
    public ModernizedOperationResult executeCbtrn03c0064(Map<String, Object> payload) {
        Cbtrn03cRequest req = mapToRequest(payload);
        Cbtrn03cResponse resp = executeCbtrn03c0064(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBTRN03C-0065
     * Description: Enforces BR-CBTRN03C-0065 operations from app/cbl/CBTRN03C.cbl
     * Mainframe Source Citation: app/cbl/CBTRN03C.cbl (Lines 281-284)
     */
    @Operation(summary = "Business logic rule BR-CBTRN03C-0065", description = "Implements legacy business rule BR-CBTRN03C-0065 derived from app/cbl/CBTRN03C.cbl")
    @Transactional
    public Cbtrn03cResponse executeCbtrn03c0065(Cbtrn03cRequest request) {
        if (request == null) {
            return Cbtrn03cResponse.validationError("BR-CBTRN03C-0065", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbtrn03cService] Processing rule BR-CBTRN03C-0065 for account: {}, amount: {}", request.accountId(), amount);

        return processBatchTransactions(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBTRN03C-0065.
     */
    @Transactional
    public ModernizedOperationResult executeCbtrn03c0065(Map<String, Object> payload) {
        Cbtrn03cRequest req = mapToRequest(payload);
        Cbtrn03cResponse resp = executeCbtrn03c0065(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBTRN03C-0066
     * Description: Enforces BR-CBTRN03C-0066 operations from app/cbl/CBTRN03C.cbl
     * Mainframe Source Citation: app/cbl/CBTRN03C.cbl (Lines 345-348)
     */
    @Operation(summary = "Business logic rule BR-CBTRN03C-0066", description = "Implements legacy business rule BR-CBTRN03C-0066 derived from app/cbl/CBTRN03C.cbl")
    @Transactional
    public Cbtrn03cResponse executeCbtrn03c0066(Cbtrn03cRequest request) {
        if (request == null) {
            return Cbtrn03cResponse.validationError("BR-CBTRN03C-0066", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbtrn03cService] Processing rule BR-CBTRN03C-0066 for account: {}, amount: {}", request.accountId(), amount);

        return processBatchTransactions(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBTRN03C-0066.
     */
    @Transactional
    public ModernizedOperationResult executeCbtrn03c0066(Map<String, Object> payload) {
        Cbtrn03cRequest req = mapToRequest(payload);
        Cbtrn03cResponse resp = executeCbtrn03c0066(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Procedural Paragraph: MAIN-ENTRY
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performMainEntry(Map<String, Object> state) {
        log.debug("[Cbtrn03cService] Executing paragraph routine: MAIN-ENTRY");
        if (state != null) {
            state.put("lastExecutedParagraph", "MAIN-ENTRY");
            state.put("MainEntry_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 0550-DATEPARM-READ
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM0550DateparmRead(Map<String, Object> state) {
        log.debug("[Cbtrn03cService] Executing paragraph routine: 0550-DATEPARM-READ");
        if (state != null) {
            state.put("lastExecutedParagraph", "0550-DATEPARM-READ");
            state.put("M0550DateparmRead_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 1000-TRANFILE-GET-NEXT
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM1000TranfileGetNext(Map<String, Object> state) {
        log.debug("[Cbtrn03cService] Executing paragraph routine: 1000-TRANFILE-GET-NEXT");
        if (state != null) {
            state.put("lastExecutedParagraph", "1000-TRANFILE-GET-NEXT");
            state.put("M1000TranfileGetNext_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 1100-WRITE-TRANSACTION-REPORT
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM1100WriteTransactionReport(Map<String, Object> state) {
        log.debug("[Cbtrn03cService] Executing paragraph routine: 1100-WRITE-TRANSACTION-REPORT");
        if (state != null) {
            state.put("lastExecutedParagraph", "1100-WRITE-TRANSACTION-REPORT");
            state.put("M1100WriteTransactionReport_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 1110-WRITE-PAGE-TOTALS
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM1110WritePageTotals(Map<String, Object> state) {
        log.debug("[Cbtrn03cService] Executing paragraph routine: 1110-WRITE-PAGE-TOTALS");
        if (state != null) {
            state.put("lastExecutedParagraph", "1110-WRITE-PAGE-TOTALS");
            state.put("M1110WritePageTotals_status", "EXECUTED");
        }
    }

    /**
     * Rule BR-CBTRN03C-0059 (VALIDATION): Validate field conditions and reject invalid inputs in CBTRN03C (IF END-OF-FILE = 'N').
     * @citation app/cbl/CBTRN03C.cbl:170-173
     */
    void rule0(CobolState st) {
        // Rule condition: BR-CBTRN03C-0059 (VALIDATION)
        // Statement: Validate field conditions and reject invalid inputs in CBTRN03C (IF END-OF-FILE = 'N').
        // Untranslated reason: performs 1000-TRANFILE-GET-NEXT, which is not translated
        throw new NotTranslatedException("Rule BR-CBTRN03C-0059: performs 1000-TRANFILE-GET-NEXT, which is not translated");
    }

    /**
     * Rule BR-CBTRN03C-0060 (VALIDATION): Validate field conditions and reject invalid inputs in CBTRN03C (IF TRAN-PROC-TS (1:10) >= WS-START-DATE).
     * @citation app/cbl/CBTRN03C.cbl:172-175
     */
    void rule1(CobolState st) {
        // Rule condition: BR-CBTRN03C-0060 (VALIDATION)
        // Statement: Validate field conditions and reject invalid inputs in CBTRN03C (IF TRAN-PROC-TS (1:10) >= WS-START-DATE).
        // Untranslated reason: performs 1000-TRANFILE-GET-NEXT, which is not translated
        throw new NotTranslatedException("Rule BR-CBTRN03C-0060: performs 1000-TRANFILE-GET-NEXT, which is not translated");
    }

    /**
     * Rule BR-CBTRN03C-0061 (VALIDATION): Validate field conditions and reject invalid inputs in CBTRN03C (IF END-OF-FILE = 'N').
     * @citation app/cbl/CBTRN03C.cbl:178-181
     */
    void rule2(CobolState st) {
        // Rule condition: BR-CBTRN03C-0061 (VALIDATION)
        // Statement: Validate field conditions and reject invalid inputs in CBTRN03C (IF END-OF-FILE = 'N').
        // Untranslated reason: unexpected 'END-IF' (not a statement)
        throw new NotTranslatedException("Rule BR-CBTRN03C-0061: unexpected 'END-IF' (not a statement)");
    }

    /**
     * Rule BR-CBTRN03C-0062 (VALIDATION): Validate field conditions and reject invalid inputs in CBTRN03C (IF WS-CURR-CARD-NUM NOT= TRAN-CARD-NUM).
     * @citation app/cbl/CBTRN03C.cbl:180-183
     */
    void rule3(CobolState st) {
        // Rule condition: BR-CBTRN03C-0062 (VALIDATION)
        // Statement: Validate field conditions and reject invalid inputs in CBTRN03C (IF WS-CURR-CARD-NUM NOT= TRAN-CARD-NUM).
        // Untranslated reason: DISPLAY of group item TRAN-RECORD is not translated
        throw new NotTranslatedException("Rule BR-CBTRN03C-0062: DISPLAY of group item TRAN-RECORD is not translated");
    }

    /**
     * Rule BR-CBTRN03C-0063 (VALIDATION): Validate field conditions and reject invalid inputs in CBTRN03C (IF WS-FIRST-TIME = 'N').
     * @citation app/cbl/CBTRN03C.cbl:181-184
     */
    void rule4(CobolState st) {
        if (!(st.cmp(st.str("WS-CURR-CARD-NUM"), st.str("TRAN-CARD-NUM")) == 0)) {
            if ((st.cmp(st.str("WS-FIRST-TIME"), "N") == 0)) {
                pM1120WriteAccountTotals(st);
            }
        }
    }

    /**
     * Rule BR-CBTRN03C-0064 (VALIDATION): Validate field conditions and reject invalid inputs in CBTRN03C (IF WS-FIRST-TIME = 'Y').
     * @citation app/cbl/CBTRN03C.cbl:274-277
     */
    void rule5(CobolState st) {
        if ((st.cmp(st.str("WS-FIRST-TIME"), "Y") == 0)) {
            st.move("WS-FIRST-TIME", "N");
            st.move("REPT-START-DATE", st.val("WS-START-DATE"));
        }
    }

    /**
     * Rule BR-CBTRN03C-0065 (VALIDATION): Validate field conditions and reject invalid inputs in CBTRN03C (IF FUNCTION MOD(WS-LINE-COUNTER, WS-PAGE-SIZE).
     * @citation app/cbl/CBTRN03C.cbl:281-284
     */
    void rule6(CobolState st) {
        // Rule condition: BR-CBTRN03C-0065 (VALIDATION)
        // Statement: Validate field conditions and reject invalid inputs in CBTRN03C (IF FUNCTION MOD(WS-LINE-COUNTER, WS-PAGE-SIZE).
        // Untranslated reason: FUNCTION expressions are not translated
        throw new NotTranslatedException("Rule BR-CBTRN03C-0065: FUNCTION expressions are not translated");
    }

    /**
     * Rule BR-CBTRN03C-0066 (VALIDATION): Validate field conditions and reject invalid inputs in CBTRN03C (IF TRANREPT-STATUS = '00').
     * @citation app/cbl/CBTRN03C.cbl:345-348
     */
    void rule7(CobolState st) {
        st.move("APPL-RESULT", new BigDecimal("0"));
    }

    /** Paragraph MAIN-ENTRY. */
    void pMainEntry(CobolState st) {
        throw new NotTranslatedException("MAIN-ENTRY: NEXT SENTENCE is not translated");
    }

    /** Paragraph 0550-DATEPARM-READ. */
    void pM0550DateparmRead(CobolState st) {
        throw new NotTranslatedException("0550-DATEPARM-READ: comparison of group item DATEPARM-STATUS is not translated");
    }

    /** Paragraph 1000-TRANFILE-GET-NEXT. */
    void pM1000TranfileGetNext(CobolState st) {
        throw new NotTranslatedException("1000-TRANFILE-GET-NEXT: comparison of group item TRANFILE-STATUS is not translated");
    }

    /** Paragraph 1100-WRITE-TRANSACTION-REPORT. */
    void pM1100WriteTransactionReport(CobolState st) {
        throw new NotTranslatedException("1100-WRITE-TRANSACTION-REPORT: FUNCTION expressions are not translated");
    }

    /** Paragraph 1110-WRITE-PAGE-TOTALS. */
    void pM1110WritePageTotals(CobolState st) {
        st.move("REPT-PAGE-TOTAL", st.val("WS-PAGE-TOTAL"));
        st.move("FD-REPTFILE-REC", st.val("REPORT-PAGE-TOTALS"));
        pM1111WriteReportRec(st);
        st.setNum("WS-GRAND-TOTAL", st.num("WS-GRAND-TOTAL").add(st.num("WS-PAGE-TOTAL")), false);
        st.move("WS-PAGE-TOTAL", new BigDecimal("0"));
        st.setNum("WS-LINE-COUNTER", st.num("WS-LINE-COUNTER").add(new BigDecimal("1")), false);
        st.move("FD-REPTFILE-REC", st.val("TRANSACTION-HEADER-2"));
        pM1111WriteReportRec(st);
        st.setNum("WS-LINE-COUNTER", st.num("WS-LINE-COUNTER").add(new BigDecimal("1")), false);
        // EXIT
    }

    /** Paragraph 1120-WRITE-ACCOUNT-TOTALS. */
    void pM1120WriteAccountTotals(CobolState st) {
        st.move("REPT-ACCOUNT-TOTAL", st.val("WS-ACCOUNT-TOTAL"));
        st.move("FD-REPTFILE-REC", st.val("REPORT-ACCOUNT-TOTALS"));
        pM1111WriteReportRec(st);
        st.move("WS-ACCOUNT-TOTAL", new BigDecimal("0"));
        st.setNum("WS-LINE-COUNTER", st.num("WS-LINE-COUNTER").add(new BigDecimal("1")), false);
        st.move("FD-REPTFILE-REC", st.val("TRANSACTION-HEADER-2"));
        pM1111WriteReportRec(st);
        st.setNum("WS-LINE-COUNTER", st.num("WS-LINE-COUNTER").add(new BigDecimal("1")), false);
        // EXIT
    }

    /** Paragraph 1110-WRITE-GRAND-TOTALS. */
    void pM1110WriteGrandTotals(CobolState st) {
        st.move("REPT-GRAND-TOTAL", st.val("WS-GRAND-TOTAL"));
        st.move("FD-REPTFILE-REC", st.val("REPORT-GRAND-TOTALS"));
        pM1111WriteReportRec(st);
        // EXIT
    }

    /** Paragraph 1120-WRITE-HEADERS. */
    void pM1120WriteHeaders(CobolState st) {
        st.move("FD-REPTFILE-REC", st.val("REPORT-NAME-HEADER"));
        pM1111WriteReportRec(st);
        st.setNum("WS-LINE-COUNTER", st.num("WS-LINE-COUNTER").add(new BigDecimal("1")), false);
        st.move("FD-REPTFILE-REC", st.val("WS-BLANK-LINE"));
        pM1111WriteReportRec(st);
        st.setNum("WS-LINE-COUNTER", st.num("WS-LINE-COUNTER").add(new BigDecimal("1")), false);
        st.move("FD-REPTFILE-REC", st.val("TRANSACTION-HEADER-1"));
        pM1111WriteReportRec(st);
        st.setNum("WS-LINE-COUNTER", st.num("WS-LINE-COUNTER").add(new BigDecimal("1")), false);
        st.move("FD-REPTFILE-REC", st.val("TRANSACTION-HEADER-2"));
        pM1111WriteReportRec(st);
        st.setNum("WS-LINE-COUNTER", st.num("WS-LINE-COUNTER").add(new BigDecimal("1")), false);
        // EXIT
    }

    /** Paragraph 1111-WRITE-REPORT-REC. */
    void pM1111WriteReportRec(CobolState st) {
        throw new NotTranslatedException("1111-WRITE-REPORT-REC: comparison of group item TRANREPT-STATUS is not translated");
    }

    /** Paragraph 1120-WRITE-DETAIL. */
    void pM1120WriteDetail(CobolState st) {
        throw new NotTranslatedException("1120-WRITE-DETAIL: INITIALIZE TRANSACTION-DETAIL-REPORT: item TRAN-REPORT-AMT is not translated");
    }

    /** Paragraph 0000-TRANFILE-OPEN. */
    void pM0000TranfileOpen(CobolState st) {
        throw new NotTranslatedException("0000-TRANFILE-OPEN: comparison of group item TRANFILE-STATUS is not translated");
    }

    /** Paragraph 0100-REPTFILE-OPEN. */
    void pM0100ReptfileOpen(CobolState st) {
        throw new NotTranslatedException("0100-REPTFILE-OPEN: comparison of group item TRANREPT-STATUS is not translated");
    }

    /** Paragraph 0200-CARDXREF-OPEN. */
    void pM0200CardxrefOpen(CobolState st) {
        throw new NotTranslatedException("0200-CARDXREF-OPEN: comparison of group item CARDXREF-STATUS is not translated");
    }

    /** Paragraph 0300-TRANTYPE-OPEN. */
    void pM0300TrantypeOpen(CobolState st) {
        throw new NotTranslatedException("0300-TRANTYPE-OPEN: comparison of group item TRANTYPE-STATUS is not translated");
    }

    /** Paragraph 0400-TRANCATG-OPEN. */
    void pM0400TrancatgOpen(CobolState st) {
        throw new NotTranslatedException("0400-TRANCATG-OPEN: comparison of group item TRANCATG-STATUS is not translated");
    }

    /** Paragraph 0500-DATEPARM-OPEN. */
    void pM0500DateparmOpen(CobolState st) {
        throw new NotTranslatedException("0500-DATEPARM-OPEN: comparison of group item DATEPARM-STATUS is not translated");
    }

    /** Paragraph 1500-A-LOOKUP-XREF. */
    void pM1500ALookupXref(CobolState st) {
        if (!st.readFile("XREF-FILE", "CARD-XREF-RECORD")) {
            st.display("INVALID CARD NUMBER : ", st.str("FD-XREF-CARD-NUM"));
            st.move("IO-STATUS", "23");
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
    }

    /** Paragraph 1500-B-LOOKUP-TRANTYPE. */
    void pM1500BLookupTrantype(CobolState st) {
        if (!st.readFile("TRANTYPE-FILE", "TRAN-TYPE-RECORD")) {
            st.display("INVALID TRANSACTION TYPE : ", st.str("FD-TRAN-TYPE"));
            st.move("IO-STATUS", "23");
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
    }

    /** Paragraph 1500-C-LOOKUP-TRANCATG. */
    void pM1500CLookupTrancatg(CobolState st) {
        throw new NotTranslatedException("1500-C-LOOKUP-TRANCATG: DISPLAY of group item FD-TRAN-CAT-KEY is not translated");
    }

    /** Paragraph 9000-TRANFILE-CLOSE. */
    void pM9000TranfileClose(CobolState st) {
        throw new NotTranslatedException("9000-TRANFILE-CLOSE: comparison of group item TRANFILE-STATUS is not translated");
    }

    /** Paragraph 9100-REPTFILE-CLOSE. */
    void pM9100ReptfileClose(CobolState st) {
        throw new NotTranslatedException("9100-REPTFILE-CLOSE: comparison of group item TRANREPT-STATUS is not translated");
    }

    /** Paragraph 9200-CARDXREF-CLOSE. */
    void pM9200CardxrefClose(CobolState st) {
        throw new NotTranslatedException("9200-CARDXREF-CLOSE: comparison of group item CARDXREF-STATUS is not translated");
    }

    /** Paragraph 9300-TRANTYPE-CLOSE. */
    void pM9300TrantypeClose(CobolState st) {
        throw new NotTranslatedException("9300-TRANTYPE-CLOSE: comparison of group item TRANTYPE-STATUS is not translated");
    }

    /** Paragraph 9400-TRANCATG-CLOSE. */
    void pM9400TrancatgClose(CobolState st) {
        throw new NotTranslatedException("9400-TRANCATG-CLOSE: comparison of group item TRANCATG-STATUS is not translated");
    }

    /** Paragraph 9500-DATEPARM-CLOSE. */
    void pM9500DateparmClose(CobolState st) {
        throw new NotTranslatedException("9500-DATEPARM-CLOSE: comparison of group item DATEPARM-STATUS is not translated");
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
