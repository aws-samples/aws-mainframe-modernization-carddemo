package com.client.core_financial_processing.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.DecimalMin;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Map;

/**
 * Modernized Request DTO for legacy COBOL module: CBSTM03A
 * Mainframe Source: app/cbl/CBSTM03A.CBL
 */
@Schema(description = "Request payload for Cbstm03aService domain operations")
public record Cbstm03aRequest(
    @Schema(description = "Target Account Identifier (COBOL PIC X(11))", example = "00000000001")
    @NotBlank(message = "accountId must not be blank")
    @Size(max = 20, message = "accountId exceeds max length")
    String accountId,

    @Schema(description = "Transaction / Execution Amount (COBOL PIC S9(9)V99)", example = "100.00")
    @NotNull(message = "amount is required")
    BigDecimal amount,

    @Schema(description = "Transaction code or operation subtype", example = "DEBIT")
    String transactionType,

    @Schema(description = "Correlation or reference tracking number", example = "TXN-98214")
    String referenceNumber,

    @Schema(description = "Target 16-digit Card Number for CardDemo operations", example = "1111222233334444")
    String cardNumber,

    @Schema(description = "Current cyclical or account balance for balance/posting evaluation", example = "1000.00")
    BigDecimal currentBalance,

    @Schema(description = "Assigned credit limit", example = "1500.00")
    BigDecimal creditLimit,

    @Schema(description = "Monthly or disclosure interest rate percentage", example = "12.00")
    BigDecimal interestRate,

    @Schema(description = "Transaction effective date for expiration checks", example = "2026-04-15")
    LocalDate transactionDate,

    @Schema(description = "Account / card expiration date", example = "2027-12-31")
    LocalDate expirationDate,

    @Schema(description = "Input date string for calendar/CEEDAYS date conversion validation", example = "2026-04-15")
    String dateString,

    @Schema(description = "Input date format pattern", example = "YYYY-MM-DD")
    String dateFormat,

    @Schema(description = "Wait duration in centiseconds (1/100 second) for delay routines", example = "100")
    Integer waitCentiseconds,

    // Customer Domain Fields (CBCUS01C, COACTUPC, COACTVWC)
    @Schema(description = "Customer Identifier (COBOL PIC X(9))", example = "000000001")
    String customerId,

    @Schema(description = "Customer First Name", example = "JOHN")
    String firstName,

    @Schema(description = "Customer Last Name", example = "DOE")
    String lastName,

    @Schema(description = "Customer Social Security Number", example = "123456789")
    String ssn,

    @Schema(description = "Customer Street Address", example = "123 MAIN ST")
    String address,

    @Schema(description = "Customer City", example = "NEW YORK")
    String city,

    @Schema(description = "Customer State Code", example = "NY")
    String state,

    @Schema(description = "Customer Zip Code", example = "10001")
    String zip,

    @Schema(description = "Customer Phone Number", example = "5551234567")
    String phoneNumber,

    // Security & Auth Domain Fields (COSGN00C)
    @Schema(description = "User Identifier for Sign-On", example = "USER0001")
    String userId,

    @Schema(description = "User Secret Password", example = "PASSWORD123")
    String password,

    // Menu Domain Fields (COMEN01C)
    @Schema(description = "Selected CICS Menu Option (1-8)", example = "1")
    Integer selectedOption,

    // Statement Domain Fields (CBSTM03A, CBSTM03B)
    @Schema(description = "Statement Month (1-12)", example = "4")
    Integer statementMonth,

    @Schema(description = "Statement Year", example = "2026")
    Integer statementYear,

    @Schema(description = "Statement Starting Balance", example = "1000.00")
    BigDecimal startingBalance,

    @Schema(description = "Total Debits in Statement Period", example = "250.00")
    BigDecimal totalDebits,

    @Schema(description = "Total Credits in Statement Period", example = "500.00")
    BigDecimal totalCredits,

    // Batch Processing Fields (CBTRN03C)
    @Schema(description = "Total Records Processed in Batch", example = "100")
    Integer recordsProcessed,

    @Schema(description = "Total Records Accepted in Batch", example = "95")
    Integer recordsAccepted,

    @Schema(description = "Total Records Rejected in Batch", example = "5")
    Integer recordsRejected,

    @Schema(description = "Card Status (ACTIVE, BLOCKED, EXPIRED)", example = "ACTIVE")
    String cardStatus,

    @Schema(description = "Additional dynamic business parameters")
    Map<String, Object> parameters
) {
    public Cbstm03aRequest {
        if (parameters == null) {
            parameters = Map.of();
        }
    }

    public Cbstm03aRequest(String accountId, BigDecimal amount, String transactionType, String referenceNumber, Map<String, Object> parameters) {
        this(accountId, amount, transactionType, referenceNumber, null, null, null, null, null, null, null, null, null,
             null, null, null, null, null, null, null, null, null,
             null, null, null, null, null, null, null, null,
             null, null, null, null, parameters);
    }

    public Cbstm03aRequest(String accountId, BigDecimal amount, String transactionType, String referenceNumber,
                          String cardNumber, BigDecimal currentBalance, BigDecimal creditLimit, BigDecimal interestRate,
                          LocalDate transactionDate, LocalDate expirationDate, String dateString, String dateFormat,
                          Integer waitCentiseconds, Map<String, Object> parameters) {
        this(accountId, amount, transactionType, referenceNumber, cardNumber, currentBalance, creditLimit, interestRate,
             transactionDate, expirationDate, dateString, dateFormat, waitCentiseconds,
             null, null, null, null, null, null, null, null, null,
             null, null, null, null, null, null, null, null,
             null, null, null, null, parameters);
    }

    public static Cbstm03aRequest defaultBaseline() {
        return new Cbstm03aRequest("", BigDecimal.ZERO, "DEFAULT", "REF-DEFAULT", Map.of());
    }
}
