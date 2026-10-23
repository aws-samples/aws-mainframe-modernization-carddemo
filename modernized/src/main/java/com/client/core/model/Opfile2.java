package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;
import java.math.BigDecimal;

/**
 * Modernized JPA Entity generated from Copybook: OPFILE2
 * @citation OPFILE2 (source not found)
 * 20 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "opfile2")
public class Opfile2 implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Unique Primary Key UUID
     * PIC clause: X(36)
     */
    @Id
    @Column(name = "id", nullable = false)
    @JsonProperty("ID")
    private String id;

    /**
     * Business Record Identifier
     * PIC clause: X(20)
     */
    @Column(name = "record_id")
    @JsonProperty("RECORD_ID")
    private String recordId;

    /**
     * Entity Type Classification
     * PIC clause: X(10)
     */
    @Column(name = "record_type")
    @JsonProperty("RECORD_TYPE")
    private String recordType;

    /**
     * Lifecycle Status (ACTIVE, CLOSED, PENDING)
     * PIC clause: X(10)
     */
    @Column(name = "record_status")
    @JsonProperty("RECORD_STATUS")
    private String recordStatus;

    /**
     * Legacy Mainframe Source Key Reference
     * PIC clause: X(30)
     */
    @Column(name = "primary_key_ref")
    @JsonProperty("PRIMARY_KEY_REF")
    private String primaryKeyRef;

    /**
     * Associated Financial Account
     * PIC clause: X(20)
     */
    @Column(name = "account_number")
    @JsonProperty("ACCOUNT_NUMBER")
    private String accountNumber;

    /**
     * Associated Payment Card
     * PIC clause: X(16)
     */
    @Column(name = "card_number")
    @JsonProperty("CARD_NUMBER")
    private String cardNumber;

    /**
     * Associated Customer Identification
     * PIC clause: X(20)
     */
    @Column(name = "customer_number")
    @JsonProperty("CUSTOMER_NUMBER")
    private String customerNumber;

    /**
     * External Transaction or Audit Reference
     * PIC clause: X(30)
     */
    @Column(name = "reference_number")
    @JsonProperty("REFERENCE_NUMBER")
    private String referenceNumber;

    /**
     * Monetary Value or Transaction Sum
     * PIC clause: S9(10)V99
     */
    @Column(name = "amount")
    @JsonProperty("AMOUNT")
    private BigDecimal amount;

    /**
     * Current Account / Cluster Balance
     * PIC clause: S9(10)V99
     */
    @Column(name = "balance")
    @JsonProperty("BALANCE")
    private BigDecimal balance;

    /**
     * Credit or Exposure Limit
     * PIC clause: S9(10)V99
     */
    @Column(name = "limit_amount")
    @JsonProperty("LIMIT_AMOUNT")
    private BigDecimal limitAmount;

    /**
     * Operation Type Code
     * PIC clause: X(10)
     */
    @Column(name = "transaction_type")
    @JsonProperty("TRANSACTION_TYPE")
    private String transactionType;

    /**
     * Accounting or Product Category
     * PIC clause: X(10)
     */
    @Column(name = "category_code")
    @JsonProperty("CATEGORY_CODE")
    private String categoryCode;

    /**
     * Processing Reason or Severity Code
     * PIC clause: X(10)
     */
    @Column(name = "status_code")
    @JsonProperty("STATUS_CODE")
    private String statusCode;

    /**
     * Human-readable Business Description
     * PIC clause: X(100)
     */
    @Column(name = "description")
    @JsonProperty("DESCRIPTION")
    private String description;

    /**
     * Effective Start Date (YYYY-MM-DD)
     * PIC clause: X(10)
     */
    @Column(name = "effective_date")
    @JsonProperty("EFFECTIVE_DATE")
    private String effectiveDate;

    /**
     * Expiration / Sunset Date (YYYY-MM-DD)
     * PIC clause: X(10)
     */
    @Column(name = "expiration_date")
    @JsonProperty("EXPIRATION_DATE")
    private String expirationDate;

    /**
     * Auditing Annotations and Notes
     * PIC clause: X(255)
     */
    @Column(name = "notes")
    @JsonProperty("NOTES")
    private String notes;

    /**
     * Raw Normalized Legacy EBCDIC Buffer
     * PIC clause: X(1000)
     */
    @Column(name = "raw_record_payload")
    @JsonProperty("RAW_RECORD_PAYLOAD")
    private String rawRecordPayload;

}
