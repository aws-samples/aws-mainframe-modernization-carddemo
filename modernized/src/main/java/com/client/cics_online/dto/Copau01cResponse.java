package com.client.cics_online.dto;

import io.swagger.v3.oas.annotations.media.Schema;

import java.math.BigDecimal;
import java.time.Instant;
import java.util.Map;

/**
 * Modernized Response DTO for legacy COBOL module: COPAU01C
 * Mainframe Source: app/cbl/COPAU01C.cbl
 */
@Schema(description = "Response payload for Copau01cService domain operations")
public record Copau01cResponse(
    @Schema(description = "Executed Business Rule Identifier", example = "BR-COPAU01C-0001")
    String ruleId,

    @Schema(description = "Operation Status (SUCCESS, DECLINED, VALIDATION_ERROR, UNTRANSLATED)", example = "SUCCESS")
    String status,

    @Schema(description = "Human-readable execution summary or failure rationale", example = "Transaction processed successfully")
    String message,

    @Schema(description = "Mainframe posting reason or severity code (e.g., 0, 100, 101, 102, 103, 12)", example = "0")
    int reasonCode,

    @Schema(description = "Execution severity level (0=Normal, 4=Warning, 8=Error, 12=Severe)", example = "0")
    int severity,

    @Schema(description = "Resulting Account Balance", example = "1250.75")
    BigDecimal newBalance,

    @Schema(description = "Calculated monthly interest amount (for CBACT04C)", example = "12.50")
    BigDecimal interestAmount,

    @Schema(description = "Date validation status (for CSUTLDTC)", example = "Date is valid")
    String dateValidationResult,

    @Schema(description = "Transaction Reference Identifier", example = "TXN-73910")
    String transactionReference,

    // Domain Specific Output Fields
    @Schema(description = "Customer Identifier", example = "000000001")
    String customerId,

    @Schema(description = "Customer First Name", example = "JOHN")
    String firstName,

    @Schema(description = "Customer Last Name", example = "DOE")
    String lastName,

    @Schema(description = "User Identifier", example = "USER0001")
    String userId,

    @Schema(description = "Authentication Success Flag", example = "true")
    Boolean authenticated,

    @Schema(description = "User Role / Type", example = "USER")
    String userType,

    @Schema(description = "Authentication Session Token", example = "SESS-TOKEN-1234")
    String sessionId,

    @Schema(description = "Selected Menu Option Number", example = "1")
    Integer selectedOption,

    @Schema(description = "Navigation Target Program", example = "COACTVWC")
    String targetProgram,

    @Schema(description = "Screen Title", example = "Account View")
    String screenTitle,

    @Schema(description = "Statement Net Activity", example = "250.00")
    BigDecimal netActivity,

    @Schema(description = "Statement Ending Balance", example = "1250.00")
    BigDecimal endingBalance,

    @Schema(description = "Batch Records Processed", example = "100")
    Integer recordsProcessed,

    @Schema(description = "Batch Records Accepted", example = "95")
    Integer recordsAccepted,

    @Schema(description = "Batch Records Rejected", example = "5")
    Integer recordsRejected,

    @Schema(description = "Card Status", example = "ACTIVE")
    String cardStatus,

    @Schema(description = "Execution Timestamp")
    Instant timestamp,

    @Schema(description = "COBOL state snapshot after execution")
    Map<String, Object> state
) {
    public Copau01cResponse(String ruleId, String status, String message, BigDecimal newBalance, String transactionReference) {
        this(ruleId, status, message, 0, 0, newBalance != null ? newBalance : BigDecimal.ZERO, null, null, transactionReference,
             null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null,
             Instant.now(), Map.of());
    }

    public Copau01cResponse(String ruleId, String status, String message, BigDecimal newBalance, String transactionReference, Instant timestamp) {
        this(ruleId, status, message, 0, 0, newBalance != null ? newBalance : BigDecimal.ZERO, null, null, transactionReference,
             null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null,
             timestamp, Map.of());
    }

    public Copau01cResponse(String ruleId, String status, String message, int reasonCode, int severity,
                           BigDecimal newBalance, BigDecimal interestAmount, String dateValidationResult,
                           String transactionReference, Instant timestamp, Map<String, Object> state) {
        this(ruleId, status, message, reasonCode, severity, newBalance != null ? newBalance : BigDecimal.ZERO, interestAmount, dateValidationResult, transactionReference,
             null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null,
             timestamp, state);
    }

    public BigDecimal resultingBalance() { return newBalance != null ? newBalance : (endingBalance != null ? endingBalance : BigDecimal.ZERO); }
    public String referenceNumber() { return transactionReference; }

    public static Copau01cResponse success(String ruleId, String message, BigDecimal balance, String ref) {
        return new Copau01cResponse(ruleId, "SUCCESS", message, 0, 0, balance != null ? balance : BigDecimal.ZERO, null, null, ref,
            null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null,
            Instant.now(), Map.of());
    }

    public static Copau01cResponse declined(String ruleId, String message, int reasonCode, BigDecimal balance, String ref) {
        return new Copau01cResponse(ruleId, "DECLINED", message, reasonCode, 8, balance != null ? balance : BigDecimal.ZERO, null, null, ref,
            null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null,
            Instant.now(), Map.of());
    }

    public static Copau01cResponse declined(String ruleId, String message, BigDecimal balance, String ref) {
        return new Copau01cResponse(ruleId, "DECLINED", message, 102, 8, balance != null ? balance : BigDecimal.ZERO, null, null, ref,
            null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null,
            Instant.now(), Map.of());
    }

    public static Copau01cResponse validationError(String ruleId, String message) {
        return new Copau01cResponse(ruleId, "VALIDATION_ERROR", message, 100, 8, BigDecimal.ZERO, null, null, "ERR",
            null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null,
            Instant.now(), Map.of());
    }

    public static Copau01cResponse untranslated(String ruleId, String reason) {
        return new Copau01cResponse(ruleId, "UNTRANSLATED", reason, 999, 12, BigDecimal.ZERO, null, null, "UNTRANSLATED",
            null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null,
            Instant.now(), Map.of());
    }

    public static Copau01cResponse interestResult(String ruleId, String message, int reasonCode, BigDecimal balance, BigDecimal interest, String ref) {
        return new Copau01cResponse(ruleId, "SUCCESS", message, reasonCode, 0, balance != null ? balance : BigDecimal.ZERO, interest, null, ref,
            null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null,
            Instant.now(), Map.of());
    }

    public static Copau01cResponse dateValidationResult(String ruleId, String message, int severity, String result) {
        return new Copau01cResponse(ruleId, severity == 0 ? "SUCCESS" : "VALIDATION_ERROR", message, severity, severity, BigDecimal.ZERO, null, result, "DATE-VAL",
            null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null,
            Instant.now(), Map.of());
    }

    public static Copau01cResponse customerSuccess(String ruleId, String message, String customerId, String firstName, String lastName, String address, String city, String state, String zip, String ref) {
        return new Copau01cResponse(ruleId, "SUCCESS", message, 0, 0, BigDecimal.ZERO, null, null, ref,
            customerId, firstName, lastName, null, null, null, null, null, null, null, null, null, null, null, null, null,
            Instant.now(), Map.of());
    }

    public static Copau01cResponse accountSuccess(String ruleId, String message, String accountId, String status, BigDecimal balance, BigDecimal limit, String ref) {
        return new Copau01cResponse(ruleId, "SUCCESS", message, 0, 0, balance != null ? balance : BigDecimal.ZERO, null, null, ref,
            null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, status,
            Instant.now(), Map.of());
    }

    public static Copau01cResponse xrefSuccess(String ruleId, String message, String cardNumber, String accountId, String customerId, String ref) {
        return new Copau01cResponse(ruleId, "SUCCESS", message, 0, 0, BigDecimal.ZERO, null, null, ref,
            customerId, null, null, null, null, null, null, null, null, null, null, null, null, null, null, "ACTIVE",
            Instant.now(), Map.of());
    }

    public static Copau01cResponse statementSuccess(String ruleId, String message, String accountId, BigDecimal netActivity, BigDecimal endingBalance, String ref) {
        return new Copau01cResponse(ruleId, "SUCCESS", message, 0, 0, endingBalance != null ? endingBalance : BigDecimal.ZERO, null, null, ref,
            null, null, null, null, null, null, null, null, null, null, netActivity, endingBalance, null, null, null, null,
            Instant.now(), Map.of());
    }

    public static Copau01cResponse batchSuccess(String ruleId, String message, int processed, int accepted, int rejected, String ref) {
        return new Copau01cResponse(ruleId, "SUCCESS", message, 0, 0, BigDecimal.ZERO, null, null, ref,
            null, null, null, null, null, null, null, null, null, null, null, null, processed, accepted, rejected, null,
            Instant.now(), Map.of());
    }

    public static Copau01cResponse authSuccess(String ruleId, String message, String userId, boolean authenticated, String userType, String sessionId, String ref) {
        return new Copau01cResponse(ruleId, "SUCCESS", message, 0, 0, BigDecimal.ZERO, null, null, ref,
            null, null, null, userId, authenticated, userType, sessionId, null, null, null, null, null, null, null, null, null,
            Instant.now(), Map.of());
    }

    public static Copau01cResponse authDeclined(String ruleId, String message, int code, String ref) {
        return new Copau01cResponse(ruleId, "DECLINED", message, code, 8, BigDecimal.ZERO, null, null, ref,
            null, null, null, null, false, null, null, null, null, null, null, null, null, null, null, null,
            Instant.now(), Map.of());
    }

    public static Copau01cResponse menuSuccess(String ruleId, String message, int option, String targetProgram, String screenTitle, String ref) {
        return new Copau01cResponse(ruleId, "SUCCESS", message, 0, 0, BigDecimal.ZERO, null, null, ref,
            null, null, null, null, null, null, null, option, targetProgram, screenTitle, null, null, null, null, null, null,
            Instant.now(), Map.of());
    }

    public static Copau01cResponse menuDeclined(String ruleId, String message, int code, String ref) {
        return new Copau01cResponse(ruleId, "DECLINED", message, code, 8, BigDecimal.ZERO, null, null, ref,
            null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null,
            Instant.now(), Map.of());
    }

    public static Copau01cResponse cardSuccess(String ruleId, String message, String cardNumber, String status, String ref) {
        return new Copau01cResponse(ruleId, "SUCCESS", message, 0, 0, BigDecimal.ZERO, null, null, ref,
            null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, status,
            Instant.now(), Map.of());
    }
}
