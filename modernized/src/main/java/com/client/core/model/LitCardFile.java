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
 * Modernized JPA Entity generated from Copybook: LIT-CARD-FILE
 * @citation LIT-CARD-FILE (source not found)
 * 9 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "lit_card_file")
public class LitCardFile implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 16-digit Primary Account Number (PAN)
     * PIC clause: X(16)
     */
    @Id
    @Column(name = "card_num", nullable = false)
    @JsonProperty("CARD_NUM")
    private String cardNum;

    /**
     * Associated Financial Account ID
     * PIC clause: 9(11)
     */
    @Column(name = "acct_id")
    @JsonProperty("ACCT_ID")
    private Long acctId;

    /**
     * Associated Customer ID
     * PIC clause: 9(09)
     */
    @Column(name = "cust_id")
    @JsonProperty("CUST_ID")
    private Integer custId;

    /**
     * Card Lifecycle Status (ACTIVE, BLOCKED, EXPIRED)
     * PIC clause: X(01)
     */
    @Column(name = "card_status")
    @JsonProperty("CARD_STATUS")
    private String cardStatus;

    /**
     * Card Brand (VISA, MASTERCARD, AMEX)
     * PIC clause: X(10)
     */
    @Column(name = "card_type")
    @JsonProperty("CARD_TYPE")
    private String cardType;

    /**
     * Embossed Cardholder Name
     * PIC clause: X(50)
     */
    @Column(name = "card_embossed_name")
    @JsonProperty("CARD_EMBOSSED_NAME")
    private String cardEmbossedName;

    /**
     * Card Expiration Date (MM/YY or YYYY-MM-DD)
     * PIC clause: X(10)
     */
    @Column(name = "card_exp_date")
    @JsonProperty("CARD_EXP_DATE")
    private String cardExpDate;

    /**
     * Issuance Date
     * PIC clause: X(10)
     */
    @Column(name = "card_issued_date")
    @JsonProperty("CARD_ISSUED_DATE")
    private String cardIssuedDate;

    /**
     * Card Level Credit Limit
     * PIC clause: S9(10)V99
     */
    @Column(name = "credit_limit")
    @JsonProperty("CREDIT_LIMIT")
    private BigDecimal creditLimit;

}
