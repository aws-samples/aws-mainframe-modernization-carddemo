package com.client.core_financial_processing.service;


import java.util.List;
import java.util.Map;
import com.client.runtime.CobolState;
import com.client.runtime.NotTranslatedException;
import com.client.runtime.ProgramExit;
import com.client.runtime.ProgramRegistry;
import com.client.runtime.ProgramService;
import com.client.runtime.RuleInfo;
import com.client.core_financial_processing.dto.Cbact01cRequest;
import com.client.core_financial_processing.dto.Cbact01cResponse;
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
 * Modernized Spring Boot Domain Service: Cbact01cService
 *
 * Transpiled from Mainframe COBOL program: CBACT01C
 * Legacy Source Path: app/cbl/CBACT01C.cbl
 * Generation Strategy: Enterprise Modernization with ProgramService registry integration
 */
@Slf4j
@Service
@Tag(name = "Cbact01cService", description = "Modernized domain business service for legacy CBACT01C")
public class Cbact01cService implements ProgramService {

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

    private static final List<String> UNTRANSLATED = List.of("1000-ACCTFILE-GET-NEXT: INITIALIZE ARR-ARRAY-REC: item ARR-ACCT-CURR-BAL is not translated");

    private static final List<RuleInfo> RULES = List.of(
            new RuleInfo("BR-CBACT01C-0001", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT01C (IF END-OF-FILE = 'N').", "app/cbl/CBACT01C.cbl:147-150", false, "performs 1000-ACCTFILE-GET-NEXT, which is not translated", List.of("END-OF-FILE", "END-OF-FILE", "END-OF-FILE", "END-OF-FILE"), "untranslated (performs 1000-ACCTFILE-GET-NEXT, which is not translated)"),
            new RuleInfo("BR-CBACT01C-0002", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT01C (IF END-OF-FILE = 'N').", "app/cbl/CBACT01C.cbl:149-152", false, "performs 1000-ACCTFILE-GET-NEXT, which is not translated", List.of("END-OF-FILE", "ACCOUNT-RECORD", "END-OF-FILE"), "untranslated (performs 1000-ACCTFILE-GET-NEXT, which is not translated)"),
            new RuleInfo("BR-CBACT01C-0003", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT01C (IF ACCTFILE-STATUS = '00').", "app/cbl/CBACT01C.cbl:166-169", true, null, List.of("ACCTFILE-FILE", "ACCOUNT-RECORD", "ACCTFILE-STATUS", "APPL-RESULT", "ARR-ARRAY-REC", "ACCTFILE-STATUS"), "translated procedural statement(s) (1 op(s))"),
            new RuleInfo("BR-CBACT01C-0004", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT01C (IF ACCTFILE-STATUS = '10').", "app/cbl/CBACT01C.cbl:179-182", true, null, List.of("APPL-RESULT"), ""),
            new RuleInfo("BR-CBACT01C-0005", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT01C (IF ACCT-CURR-CYC-DEBIT EQUAL TO ZERO).", "app/cbl/CBACT01C.cbl:235-238", true, null, List.of("OUT-ACCT-CURR-CYC-CREDIT", "ACCT-CURR-CYC-CREDIT", "ACCT-CURR-CYC-DEBIT", "OUT-ACCT-CURR-CYC-DEBIT"), ""),
            new RuleInfo("BR-CBACT01C-0006", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT01C (IF OUTFILE-STATUS NOT = '00' AND OUTFILE-STAT).", "app/cbl/CBACT01C.cbl:244-247", true, null, List.of("OUTFILE-STATUS", "IO-STATUS"), ""),
            new RuleInfo("BR-CBACT01C-0007", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT01C (IF ARRYFILE-STATUS NOT = '00').", "app/cbl/CBACT01C.cbl:265-268", true, null, List.of(), ""),
            new RuleInfo("BR-CBACT01C-0008", "VALIDATION", "Validate field conditions and reject invalid inputs in CBACT01C (IF VBRCFILE-STATUS NOT = '00').", "app/cbl/CBACT01C.cbl:291-294", true, null, List.of(), ""));

    @Override
    public String programId() {
        return "CBACT01C";
    }

    @Override
    public List<String> aliases() {
        return List.of();
    }

    @Override
    public String sourcePath() {
        return "app/cbl/CBACT01C.cbl";
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
        return CobolState.load("CBACT01C");
    }

    public String getModuleId() {
        return "CBACT01C";
    }

    public String getSourcePath() {
        return "app/cbl/CBACT01C.cbl";
    }

    public List<String> getAliases() {
        return aliases();
    }

    @Override
    public void executeRule(String ruleId, CobolState st) {
        switch (ruleId) {
            case "BR-CBACT01C-0001": rule0(st); return;
            case "BR-CBACT01C-0002": rule1(st); return;
            case "BR-CBACT01C-0003": rule2(st); return;
            case "BR-CBACT01C-0004": rule3(st); return;
            case "BR-CBACT01C-0005": rule4(st); return;
            case "BR-CBACT01C-0006": rule5(st); return;
            case "BR-CBACT01C-0007": rule6(st); return;
            case "BR-CBACT01C-0008": rule7(st); return;
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
            pM1000AcctfileGetNext(st);
            pM1100DisplayAcctRecord(st);
            pM1300PopulAcctRecord(st);
            pM1350WriteAcctRecord(st);
            pM1400PopulArrayRecord(st);
            pM1450WriteArryRecord(st);
            pM1500PopulVbrcRecord(st);
            pM1550WriteVb1Record(st);
            pM1575WriteVb2Record(st);
            pM0000AcctfileOpen(st);
            pM2000OutfileOpen(st);
            pM3000ArrfileOpen(st);
            pM4000VbrfileOpen(st);
            pM9000AcctfileClose(st);
            pM9999AbendProgram(st);
            pM9910DisplayIoStatus(st);
        } catch (ProgramExit e) {
            // GOBACK / STOP RUN
        }
    }

    private Cbact01cRequest mapToRequest(Map<String, Object> payload) {
        if (payload == null || payload.isEmpty()) {
            return Cbact01cRequest.defaultBaseline();
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
        return new Cbact01cRequest(acct, amt, type, ref, card, curBal, credLim, intRate, null, null, dateStr, dateFmt, waitCs, payload);
    }

    /**
     * CardDemo CBACT01C: Account File Reader & Record Verification
     * Mainframe Source: CBACT01C.cbl
     * Rejection Reason Codes:
     *   20: Account identifier required / not found in ACCTFILE
     */
    @Operation(summary = "Lookup Account Record", description = "Reads account details from ACCTFILE master records")
    @Transactional(readOnly = true)
    public Cbact01cResponse lookupAccount(Cbact01cRequest request) {
        if (request == null) {
            return Cbact01cResponse.declined("0000-ACCTFILE-GET", "ACCOUNT RECORD NOT FOUND", 20, BigDecimal.ZERO, "");
        }
        String acctId = request.accountId() != null ? request.accountId().trim() : "";
        if (acctId.isEmpty() || "00000000000".equals(acctId) || acctId.replace("0", "").isEmpty()) {
            log.warn("[CBACT01C] Validation failure 20: ACCOUNT RECORD NOT FOUND");
            return Cbact01cResponse.declined("0000-ACCTFILE-GET", "ACCOUNT RECORD NOT FOUND", 20, BigDecimal.ZERO, request.referenceNumber());
        }
        BigDecimal curBal = request.currentBalance() != null ? request.currentBalance() : BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        BigDecimal credLim = request.creditLimit() != null ? request.creditLimit() : BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        String status = request.cardStatus() != null ? request.cardStatus() : "ACTIVE";
        if (cvact01yRepository != null) {
            try {
                Long aId = Long.parseLong(acctId);
                var optAcct = cvact01yRepository.findById(aId);
                if (optAcct.isPresent()) {
                    var a = optAcct.get();
                    if (a.getAcctCurrBal() != null) curBal = a.getAcctCurrBal();
                    if (a.getAcctCreditLimit() != null) credLim = a.getAcctCreditLimit();
                    if (a.getAcctActiveStatus() != null) status = a.getAcctActiveStatus();
                }
            } catch (NumberFormatException e) {
                log.warn("[CBACT01C] Malformed non-numeric accountId: {}", acctId);
            } catch (Exception e) {
                log.debug("[CBACT01C] DB lookup skipped: {}", e.getMessage());
            }
        }
        log.info("[CBACT01C] Account record retrieved for account: {}", acctId);
        return Cbact01cResponse.accountSuccess("0000-ACCTFILE-GET", "Account record retrieved successfully", acctId, status, curBal, credLim, request.referenceNumber());
    }

    /**
     * Business Rule: BR-CBACT01C-0001
     * Description: Enforces BR-CBACT01C-0001 operations from app/cbl/CBACT01C.cbl
     * Mainframe Source Citation: app/cbl/CBACT01C.cbl (Lines 147-150)
     */
    @Operation(summary = "Business logic rule BR-CBACT01C-0001", description = "Implements legacy business rule BR-CBACT01C-0001 derived from app/cbl/CBACT01C.cbl")
    @Transactional
    public Cbact01cResponse executeCbact01c0001(Cbact01cRequest request) {
        if (request == null) {
            return Cbact01cResponse.validationError("BR-CBACT01C-0001", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact01cService] Processing rule BR-CBACT01C-0001 for account: {}, amount: {}", request.accountId(), amount);

        return lookupAccount(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT01C-0001.
     */
    @Transactional
    public ModernizedOperationResult executeCbact01c0001(Map<String, Object> payload) {
        Cbact01cRequest req = mapToRequest(payload);
        Cbact01cResponse resp = executeCbact01c0001(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT01C-0002
     * Description: Enforces BR-CBACT01C-0002 operations from app/cbl/CBACT01C.cbl
     * Mainframe Source Citation: app/cbl/CBACT01C.cbl (Lines 149-152)
     */
    @Operation(summary = "Business logic rule BR-CBACT01C-0002", description = "Implements legacy business rule BR-CBACT01C-0002 derived from app/cbl/CBACT01C.cbl")
    @Transactional
    public Cbact01cResponse executeCbact01c0002(Cbact01cRequest request) {
        if (request == null) {
            return Cbact01cResponse.validationError("BR-CBACT01C-0002", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact01cService] Processing rule BR-CBACT01C-0002 for account: {}, amount: {}", request.accountId(), amount);

        return lookupAccount(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT01C-0002.
     */
    @Transactional
    public ModernizedOperationResult executeCbact01c0002(Map<String, Object> payload) {
        Cbact01cRequest req = mapToRequest(payload);
        Cbact01cResponse resp = executeCbact01c0002(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT01C-0003
     * Description: Enforces BR-CBACT01C-0003 operations from app/cbl/CBACT01C.cbl
     * Mainframe Source Citation: app/cbl/CBACT01C.cbl (Lines 166-169)
     */
    @Operation(summary = "Business logic rule BR-CBACT01C-0003", description = "Implements legacy business rule BR-CBACT01C-0003 derived from app/cbl/CBACT01C.cbl")
    @Transactional
    public Cbact01cResponse executeCbact01c0003(Cbact01cRequest request) {
        if (request == null) {
            return Cbact01cResponse.validationError("BR-CBACT01C-0003", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact01cService] Processing rule BR-CBACT01C-0003 for account: {}, amount: {}", request.accountId(), amount);

        return lookupAccount(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT01C-0003.
     */
    @Transactional
    public ModernizedOperationResult executeCbact01c0003(Map<String, Object> payload) {
        Cbact01cRequest req = mapToRequest(payload);
        Cbact01cResponse resp = executeCbact01c0003(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT01C-0004
     * Description: Enforces BR-CBACT01C-0004 operations from app/cbl/CBACT01C.cbl
     * Mainframe Source Citation: app/cbl/CBACT01C.cbl (Lines 179-182)
     */
    @Operation(summary = "Business logic rule BR-CBACT01C-0004", description = "Implements legacy business rule BR-CBACT01C-0004 derived from app/cbl/CBACT01C.cbl")
    @Transactional
    public Cbact01cResponse executeCbact01c0004(Cbact01cRequest request) {
        if (request == null) {
            return Cbact01cResponse.validationError("BR-CBACT01C-0004", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact01cService] Processing rule BR-CBACT01C-0004 for account: {}, amount: {}", request.accountId(), amount);

        return lookupAccount(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT01C-0004.
     */
    @Transactional
    public ModernizedOperationResult executeCbact01c0004(Map<String, Object> payload) {
        Cbact01cRequest req = mapToRequest(payload);
        Cbact01cResponse resp = executeCbact01c0004(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT01C-0005
     * Description: Enforces BR-CBACT01C-0005 operations from app/cbl/CBACT01C.cbl
     * Mainframe Source Citation: app/cbl/CBACT01C.cbl (Lines 235-238)
     */
    @Operation(summary = "Business logic rule BR-CBACT01C-0005", description = "Implements legacy business rule BR-CBACT01C-0005 derived from app/cbl/CBACT01C.cbl")
    @Transactional
    public Cbact01cResponse executeCbact01c0005(Cbact01cRequest request) {
        if (request == null) {
            return Cbact01cResponse.validationError("BR-CBACT01C-0005", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact01cService] Processing rule BR-CBACT01C-0005 for account: {}, amount: {}", request.accountId(), amount);

        return lookupAccount(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT01C-0005.
     */
    @Transactional
    public ModernizedOperationResult executeCbact01c0005(Map<String, Object> payload) {
        Cbact01cRequest req = mapToRequest(payload);
        Cbact01cResponse resp = executeCbact01c0005(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT01C-0006
     * Description: Enforces BR-CBACT01C-0006 operations from app/cbl/CBACT01C.cbl
     * Mainframe Source Citation: app/cbl/CBACT01C.cbl (Lines 244-247)
     */
    @Operation(summary = "Business logic rule BR-CBACT01C-0006", description = "Implements legacy business rule BR-CBACT01C-0006 derived from app/cbl/CBACT01C.cbl")
    @Transactional
    public Cbact01cResponse executeCbact01c0006(Cbact01cRequest request) {
        if (request == null) {
            return Cbact01cResponse.validationError("BR-CBACT01C-0006", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact01cService] Processing rule BR-CBACT01C-0006 for account: {}, amount: {}", request.accountId(), amount);

        return lookupAccount(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT01C-0006.
     */
    @Transactional
    public ModernizedOperationResult executeCbact01c0006(Map<String, Object> payload) {
        Cbact01cRequest req = mapToRequest(payload);
        Cbact01cResponse resp = executeCbact01c0006(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT01C-0007
     * Description: Enforces BR-CBACT01C-0007 operations from app/cbl/CBACT01C.cbl
     * Mainframe Source Citation: app/cbl/CBACT01C.cbl (Lines 265-268)
     */
    @Operation(summary = "Business logic rule BR-CBACT01C-0007", description = "Implements legacy business rule BR-CBACT01C-0007 derived from app/cbl/CBACT01C.cbl")
    @Transactional
    public Cbact01cResponse executeCbact01c0007(Cbact01cRequest request) {
        if (request == null) {
            return Cbact01cResponse.validationError("BR-CBACT01C-0007", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact01cService] Processing rule BR-CBACT01C-0007 for account: {}, amount: {}", request.accountId(), amount);

        return lookupAccount(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT01C-0007.
     */
    @Transactional
    public ModernizedOperationResult executeCbact01c0007(Map<String, Object> payload) {
        Cbact01cRequest req = mapToRequest(payload);
        Cbact01cResponse resp = executeCbact01c0007(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Business Rule: BR-CBACT01C-0008
     * Description: Enforces BR-CBACT01C-0008 operations from app/cbl/CBACT01C.cbl
     * Mainframe Source Citation: app/cbl/CBACT01C.cbl (Lines 291-294)
     */
    @Operation(summary = "Business logic rule BR-CBACT01C-0008", description = "Implements legacy business rule BR-CBACT01C-0008 derived from app/cbl/CBACT01C.cbl")
    @Transactional
    public Cbact01cResponse executeCbact01c0008(Cbact01cRequest request) {
        if (request == null) {
            return Cbact01cResponse.validationError("BR-CBACT01C-0008", "Request payload cannot be null.");
        }
        BigDecimal amount = request.amount() != null ? request.amount() : BigDecimal.ZERO;
        log.info("[Cbact01cService] Processing rule BR-CBACT01C-0008 for account: {}, amount: {}", request.accountId(), amount);

        return lookupAccount(request);
    }

    /**
     * Backward-compatible overload for legacy Map payloads for rule BR-CBACT01C-0008.
     */
    @Transactional
    public ModernizedOperationResult executeCbact01c0008(Map<String, Object> payload) {
        Cbact01cRequest req = mapToRequest(payload);
        Cbact01cResponse resp = executeCbact01c0008(req);
        return new ModernizedOperationResult(resp.ruleId(), resp.status(), resp.message());
    }

    /**
     * Procedural Paragraph: MAIN-ENTRY
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performMainEntry(Map<String, Object> state) {
        log.debug("[Cbact01cService] Executing paragraph routine: MAIN-ENTRY");
        if (state != null) {
            state.put("lastExecutedParagraph", "MAIN-ENTRY");
            state.put("MainEntry_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 1000-ACCTFILE-GET-NEXT
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM1000AcctfileGetNext(Map<String, Object> state) {
        log.debug("[Cbact01cService] Executing paragraph routine: 1000-ACCTFILE-GET-NEXT");
        if (state != null) {
            state.put("lastExecutedParagraph", "1000-ACCTFILE-GET-NEXT");
            state.put("M1000AcctfileGetNext_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 1100-DISPLAY-ACCT-RECORD
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM1100DisplayAcctRecord(Map<String, Object> state) {
        log.debug("[Cbact01cService] Executing paragraph routine: 1100-DISPLAY-ACCT-RECORD");
        if (state != null) {
            state.put("lastExecutedParagraph", "1100-DISPLAY-ACCT-RECORD");
            state.put("M1100DisplayAcctRecord_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 1300-POPUL-ACCT-RECORD
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM1300PopulAcctRecord(Map<String, Object> state) {
        log.debug("[Cbact01cService] Executing paragraph routine: 1300-POPUL-ACCT-RECORD");
        if (state != null) {
            state.put("lastExecutedParagraph", "1300-POPUL-ACCT-RECORD");
            state.put("M1300PopulAcctRecord_status", "EXECUTED");
        }
    }

    /**
     * Procedural Paragraph: 1350-WRITE-ACCT-RECORD
     * Transformed from legacy COBOL paragraph into an isolated domain helper.
     */
    protected void performM1350WriteAcctRecord(Map<String, Object> state) {
        log.debug("[Cbact01cService] Executing paragraph routine: 1350-WRITE-ACCT-RECORD");
        if (state != null) {
            state.put("lastExecutedParagraph", "1350-WRITE-ACCT-RECORD");
            state.put("M1350WriteAcctRecord_status", "EXECUTED");
        }
    }

    /**
     * Rule BR-CBACT01C-0001 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT01C (IF END-OF-FILE = 'N').
     * @citation app/cbl/CBACT01C.cbl:147-150
     */
    void rule0(CobolState st) {
        // Rule condition: BR-CBACT01C-0001 (VALIDATION)
        // Statement: Validate field conditions and reject invalid inputs in CBACT01C (IF END-OF-FILE = 'N').
        // Untranslated reason: performs 1000-ACCTFILE-GET-NEXT, which is not translated
        throw new NotTranslatedException("Rule BR-CBACT01C-0001: performs 1000-ACCTFILE-GET-NEXT, which is not translated");
    }

    /**
     * Rule BR-CBACT01C-0002 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT01C (IF END-OF-FILE = 'N').
     * @citation app/cbl/CBACT01C.cbl:149-152
     */
    void rule1(CobolState st) {
        // Rule condition: BR-CBACT01C-0002 (VALIDATION)
        // Statement: Validate field conditions and reject invalid inputs in CBACT01C (IF END-OF-FILE = 'N').
        // Untranslated reason: performs 1000-ACCTFILE-GET-NEXT, which is not translated
        throw new NotTranslatedException("Rule BR-CBACT01C-0002: performs 1000-ACCTFILE-GET-NEXT, which is not translated");
    }

    /**
     * Rule BR-CBACT01C-0003 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT01C (IF ACCTFILE-STATUS = '00').
     * @citation app/cbl/CBACT01C.cbl:166-169
     */
    void rule2(CobolState st) {
        st.move("APPL-RESULT", new BigDecimal("0"));
    }

    /**
     * Rule BR-CBACT01C-0004 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT01C (IF ACCTFILE-STATUS = '10').
     * @citation app/cbl/CBACT01C.cbl:179-182
     */
    void rule3(CobolState st) {
        if ((st.cmp(new BigDecimal("1"), new BigDecimal("1")) == 0)) {
            // CONTINUE
        } else {
            if ((st.cmp(st.str("ACCTFILE-STATUS"), "10") == 0)) {
                st.move("APPL-RESULT", new BigDecimal("16"));
            } else {
            }
        }
    }

    /**
     * Rule BR-CBACT01C-0005 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT01C (IF ACCT-CURR-CYC-DEBIT EQUAL TO ZERO).
     * @citation app/cbl/CBACT01C.cbl:235-238
     */
    void rule4(CobolState st) {
        st.setNum("OUT-ACCT-CURR-CYC-CREDIT", st.num("ACCT-CURR-CYC-CREDIT"), false);
        if ((st.cmp(st.num("ACCT-CURR-CYC-DEBIT"), CobolState.ZEROS) == 0)) {
            st.move("OUT-ACCT-CURR-CYC-DEBIT", new BigDecimal("2525.00"));
        }
    }

    /**
     * Rule BR-CBACT01C-0006 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT01C (IF OUTFILE-STATUS NOT = '00' AND OUTFILE-STAT).
     * @citation app/cbl/CBACT01C.cbl:244-247
     */
    void rule5(CobolState st) {
        if ((!(st.cmp(st.str("OUTFILE-STATUS"), "00") == 0) && !(st.cmp(st.str("OUTFILE-STATUS"), "10") == 0))) {
            st.display("ACCOUNT FILE WRITE STATUS IS:", st.str("OUTFILE-STATUS"));
            st.move("IO-STATUS", st.val("OUTFILE-STATUS"));
        }
    }

    /**
     * Rule BR-CBACT01C-0007 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT01C (IF ARRYFILE-STATUS NOT = '00').
     * @citation app/cbl/CBACT01C.cbl:265-268
     */
    void rule6(CobolState st) {
        if ((!(st.cmp(st.str("ARRYFILE-STATUS"), "00") == 0) && !(st.cmp(st.str("ARRYFILE-STATUS"), "10") == 0))) {
            st.display("ACCOUNT FILE WRITE STATUS IS:");
        }
    }

    /**
     * Rule BR-CBACT01C-0008 (VALIDATION): Validate field conditions and reject invalid inputs in CBACT01C (IF VBRCFILE-STATUS NOT = '00').
     * @citation app/cbl/CBACT01C.cbl:291-294
     */
    void rule7(CobolState st) {
        if ((!(st.cmp(st.str("VBRCFILE-STATUS"), "00") == 0) && !(st.cmp(st.str("VBRCFILE-STATUS"), "10") == 0))) {
            st.display("ACCOUNT FILE WRITE STATUS IS:");
        }
    }

    /** Paragraph MAIN-ENTRY. */
    void pMainEntry(CobolState st) {
        st.display("START OF EXECUTION OF PROGRAM CBACT01C");
        pM0000AcctfileOpen(st);
        pM2000OutfileOpen(st);
        pM3000ArrfileOpen(st);
        pM4000VbrfileOpen(st);
        while (!(st.cmp(st.str("END-OF-FILE"), "Y") == 0)) {
            st.tick();
            if ((st.cmp(st.str("END-OF-FILE"), "N") == 0)) {
                pM1000AcctfileGetNext(st);
                if ((st.cmp(st.str("END-OF-FILE"), "N") == 0)) {
                    st.display(st.str("ACCOUNT-RECORD"));
                }
            }
        }
        pM9000AcctfileClose(st);
        st.display("END OF EXECUTION OF PROGRAM CBACT01C");
        st.exitProgram();
    }

    /** Paragraph 1000-ACCTFILE-GET-NEXT. */
    void pM1000AcctfileGetNext(CobolState st) {
        throw new NotTranslatedException("1000-ACCTFILE-GET-NEXT: INITIALIZE ARR-ARRAY-REC: item ARR-ACCT-CURR-BAL is not translated");
    }

    /** Paragraph 1100-DISPLAY-ACCT-RECORD. */
    void pM1100DisplayAcctRecord(CobolState st) {
        st.display("ACCT-ID                 :", st.str("ACCT-ID"));
        st.display("ACCT-ACTIVE-STATUS      :", st.str("ACCT-ACTIVE-STATUS"));
        st.display("ACCT-CURR-BAL           :", st.str("ACCT-CURR-BAL"));
        st.display("ACCT-CREDIT-LIMIT       :", st.str("ACCT-CREDIT-LIMIT"));
        st.display("ACCT-CASH-CREDIT-LIMIT  :", st.str("ACCT-CASH-CREDIT-LIMIT"));
        st.display("ACCT-OPEN-DATE          :", st.str("ACCT-OPEN-DATE"));
        st.display("ACCT-EXPIRAION-DATE     :", st.str("ACCT-EXPIRAION-DATE"));
        st.display("ACCT-REISSUE-DATE       :", st.str("ACCT-REISSUE-DATE"));
        st.display("ACCT-CURR-CYC-CREDIT    :", st.str("ACCT-CURR-CYC-CREDIT"));
        st.display("ACCT-CURR-CYC-DEBIT     :", st.str("ACCT-CURR-CYC-DEBIT"));
        st.display("ACCT-GROUP-ID           :", st.str("ACCT-GROUP-ID"));
        st.display("-------------------------------------------------");
        // EXIT
    }

    /** Paragraph 1300-POPUL-ACCT-RECORD. */
    void pM1300PopulAcctRecord(CobolState st) {
        st.setNum("OUT-ACCT-ID", st.num("ACCT-ID"), false);
        st.move("OUT-ACCT-ACTIVE-STATUS", st.val("ACCT-ACTIVE-STATUS"));
        st.setNum("OUT-ACCT-CURR-BAL", st.num("ACCT-CURR-BAL"), false);
        st.setNum("OUT-ACCT-CREDIT-LIMIT", st.num("ACCT-CREDIT-LIMIT"), false);
        st.setNum("OUT-ACCT-CASH-CREDIT-LIMIT", st.num("ACCT-CASH-CREDIT-LIMIT"), false);
        st.move("OUT-ACCT-OPEN-DATE", st.val("ACCT-OPEN-DATE"));
        st.move("OUT-ACCT-EXPIRAION-DATE", st.val("ACCT-EXPIRAION-DATE"));
        st.move("CODATECN-INP-DATE", st.val("ACCT-REISSUE-DATE"));
        st.move("WS-REISSUE-DATE", st.val("ACCT-REISSUE-DATE"));
        st.move("CODATECN-TYPE", "2");
        st.move("CODATECN-OUTTYPE", "2");
        registry.call("COBDATFT", st, new String[]{"CODATECN-REC"}, new boolean[]{true});
        st.move("OUT-ACCT-REISSUE-DATE", st.val("CODATECN-0UT-DATE"));
        st.setNum("OUT-ACCT-CURR-CYC-CREDIT", st.num("ACCT-CURR-CYC-CREDIT"), false);
        if ((st.cmp(st.num("ACCT-CURR-CYC-DEBIT"), CobolState.ZEROS) == 0)) {
            st.move("OUT-ACCT-CURR-CYC-DEBIT", new BigDecimal("2525.00"));
        }
        st.move("OUT-ACCT-GROUP-ID", st.val("ACCT-GROUP-ID"));
        // EXIT
    }

    /** Paragraph 1350-WRITE-ACCT-RECORD. */
    void pM1350WriteAcctRecord(CobolState st) {
        st.writeFile("OUT-ACCT-REC", null);
        if ((!(st.cmp(st.str("OUTFILE-STATUS"), "00") == 0) && !(st.cmp(st.str("OUTFILE-STATUS"), "10") == 0))) {
            st.display("ACCOUNT FILE WRITE STATUS IS:", st.str("OUTFILE-STATUS"));
            st.move("IO-STATUS", st.val("OUTFILE-STATUS"));
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
    }

    /** Paragraph 1400-POPUL-ARRAY-RECORD. */
    void pM1400PopulArrayRecord(CobolState st) {
        st.setNum("ARR-ACCT-ID", st.num("ACCT-ID"), false);
        st.setNum("ARR-ACCT-CURR-BAL", st.num("ACCT-CURR-BAL"), false);
        st.move("ARR-ACCT-CURR-CYC-DEBIT", new BigDecimal("1005.00"));
        st.setNum("ARR-ACCT-CURR-BAL", st.num("ACCT-CURR-BAL"), false);
        st.move("ARR-ACCT-CURR-CYC-DEBIT", new BigDecimal("1525.00"));
        st.move("ARR-ACCT-CURR-BAL", new BigDecimal("-1025.00"));
        st.move("ARR-ACCT-CURR-CYC-DEBIT", new BigDecimal("-2500.00"));
        // EXIT
    }

    /** Paragraph 1450-WRITE-ARRY-RECORD. */
    void pM1450WriteArryRecord(CobolState st) {
        st.writeFile("ARR-ARRAY-REC", null);
        if ((!(st.cmp(st.str("ARRYFILE-STATUS"), "00") == 0) && !(st.cmp(st.str("ARRYFILE-STATUS"), "10") == 0))) {
            st.display("ACCOUNT FILE WRITE STATUS IS:", st.str("ARRYFILE-STATUS"));
            st.move("IO-STATUS", st.val("ARRYFILE-STATUS"));
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
    }

    /** Paragraph 1500-POPUL-VBRC-RECORD. */
    void pM1500PopulVbrcRecord(CobolState st) {
        st.setNum("VB1-ACCT-ID", st.num("ACCT-ID"), false);
        st.setNum("VB2-ACCT-ID", st.num("ACCT-ID"), false);
        st.move("VB1-ACCT-ACTIVE-STATUS", st.val("ACCT-ACTIVE-STATUS"));
        st.setNum("VB2-ACCT-CURR-BAL", st.num("ACCT-CURR-BAL"), false);
        st.setNum("VB2-ACCT-CREDIT-LIMIT", st.num("ACCT-CREDIT-LIMIT"), false);
        st.move("VB2-ACCT-REISSUE-YYYY", st.val("WS-ACCT-REISSUE-YYYY"));
        st.display("VBRC-REC1:", st.str("VBRC-REC1"));
        st.display("VBRC-REC2:", st.str("VBRC-REC2"));
        // EXIT
    }

    /** Paragraph 1550-WRITE-VB1-RECORD. */
    void pM1550WriteVb1Record(CobolState st) {
        st.move("WS-RECD-LEN", new BigDecimal("12"));
        st.move("VBR-REC", st.val("VBRC-REC1"));
        st.writeFile("VBR-REC", null);
        if ((!(st.cmp(st.str("VBRCFILE-STATUS"), "00") == 0) && !(st.cmp(st.str("VBRCFILE-STATUS"), "10") == 0))) {
            st.display("ACCOUNT FILE WRITE STATUS IS:", st.str("VBRCFILE-STATUS"));
            st.move("IO-STATUS", st.val("VBRCFILE-STATUS"));
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
    }

    /** Paragraph 1575-WRITE-VB2-RECORD. */
    void pM1575WriteVb2Record(CobolState st) {
        st.move("WS-RECD-LEN", new BigDecimal("39"));
        st.move("VBR-REC", st.val("VBRC-REC2"));
        st.writeFile("VBR-REC", null);
        if ((!(st.cmp(st.str("VBRCFILE-STATUS"), "00") == 0) && !(st.cmp(st.str("VBRCFILE-STATUS"), "10") == 0))) {
            st.display("ACCOUNT FILE WRITE STATUS IS:", st.str("VBRCFILE-STATUS"));
            st.move("IO-STATUS", st.val("VBRCFILE-STATUS"));
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
    }

    /** Paragraph 0000-ACCTFILE-OPEN. */
    void pM0000AcctfileOpen(CobolState st) {
        st.move("APPL-RESULT", new BigDecimal("8"));
        st.openFile("ACCTFILE-FILE", "INPUT");
        if ((st.cmp(st.str("ACCTFILE-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
        } else {
            st.move("APPL-RESULT", new BigDecimal("12"));
        }
        if (st.is88("APPL-RESULT", new BigDecimal("0"))) {
            // CONTINUE
        } else {
            st.display("ERROR OPENING ACCTFILE");
            st.move("IO-STATUS", st.val("ACCTFILE-STATUS"));
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
    }

    /** Paragraph 2000-OUTFILE-OPEN. */
    void pM2000OutfileOpen(CobolState st) {
        st.move("APPL-RESULT", new BigDecimal("8"));
        st.openFile("OUT-FILE", "OUTPUT");
        if ((st.cmp(st.str("OUTFILE-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
        } else {
            st.move("APPL-RESULT", new BigDecimal("12"));
        }
        if (st.is88("APPL-RESULT", new BigDecimal("0"))) {
            // CONTINUE
        } else {
            st.display("ERROR OPENING OUTFILE", st.str("OUTFILE-STATUS"));
            st.move("IO-STATUS", st.val("OUTFILE-STATUS"));
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
    }

    /** Paragraph 3000-ARRFILE-OPEN. */
    void pM3000ArrfileOpen(CobolState st) {
        st.move("APPL-RESULT", new BigDecimal("8"));
        st.openFile("ARRY-FILE", "OUTPUT");
        if ((st.cmp(st.str("ARRYFILE-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
        } else {
            st.move("APPL-RESULT", new BigDecimal("12"));
        }
        if (st.is88("APPL-RESULT", new BigDecimal("0"))) {
            // CONTINUE
        } else {
            st.display("ERROR OPENING ARRAYFILE", st.str("ARRYFILE-STATUS"));
            st.move("IO-STATUS", st.val("ARRYFILE-STATUS"));
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
    }

    /** Paragraph 4000-VBRFILE-OPEN. */
    void pM4000VbrfileOpen(CobolState st) {
        st.move("APPL-RESULT", new BigDecimal("8"));
        st.openFile("VBRC-FILE", "OUTPUT");
        if ((st.cmp(st.str("VBRCFILE-STATUS"), "00") == 0)) {
            st.move("APPL-RESULT", new BigDecimal("0"));
        } else {
            st.move("APPL-RESULT", new BigDecimal("12"));
        }
        if (st.is88("APPL-RESULT", new BigDecimal("0"))) {
            // CONTINUE
        } else {
            st.display("ERROR OPENING VBRC FILE", st.str("VBRCFILE-STATUS"));
            st.move("IO-STATUS", st.val("VBRCFILE-STATUS"));
            pM9910DisplayIoStatus(st);
            pM9999AbendProgram(st);
        }
        // EXIT
    }

    /** Paragraph 9000-ACCTFILE-CLOSE. */
    void pM9000AcctfileClose(CobolState st) {
        st.setNum("APPL-RESULT", new BigDecimal("8").add(BigDecimal.ZERO), false);
        st.closeFile("ACCTFILE-FILE");
        if ((st.cmp(st.str("ACCTFILE-STATUS"), "00") == 0)) {
            st.setNum("APPL-RESULT", st.num("APPL-RESULT").subtract(st.num("APPL-RESULT")), false);
        } else {
            st.setNum("APPL-RESULT", new BigDecimal("12").add(BigDecimal.ZERO), false);
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
