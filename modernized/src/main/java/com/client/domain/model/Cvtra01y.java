package com.client.domain.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;
import java.math.BigDecimal;

/**
 * Modernized JPA Entity generated from Copybook: CVTRA01Y
 * @citation cpy/CVTRA01Y.cpy
 * 4 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "cvtra01y")
public class Cvtra01y implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: TRANCAT-ACCT-ID 9(11) (line 6)
     * PIC clause: 9(11)
     */
    @Id
    @Column(name = "trancat_acct_id", nullable = false)
    @JsonProperty("TRANCAT_ACCT_ID")
    private Long trancatAcctId;

    /**
     * Original: TRANCAT-TYPE-CD X(02) (line 7)
     * PIC clause: X(02)
     */
    @Column(name = "trancat_type_cd")
    @JsonProperty("TRANCAT_TYPE_CD")
    private String trancatTypeCd;

    /**
     * Original: TRANCAT-CD 9(04) (line 8)
     * PIC clause: 9(04)
     */
    @Column(name = "trancat_cd")
    @JsonProperty("TRANCAT_CD")
    private Integer trancatCd;

    /**
     * Original: TRAN-CAT-BAL S9(09)V99 (line 9)
     * PIC clause: S9(09)V99
     */
    @Column(name = "tran_cat_bal")
    @JsonProperty("TRAN_CAT_BAL")
    private BigDecimal tranCatBal;

    /**
     * Unique Transaction Identifier
     * Note: Transaction posting attribute.
     */
    @Transient
    @JsonProperty("TRAN_ID")
    private String tranId;

    /**
     * Card PAN Used in Transaction
     * Note: Transaction posting attribute.
     */
    @Transient
    @JsonProperty("CARD_NUM")
    private String cardNum;

    /**
     * Authorized Transaction Amount
     * Note: Transaction posting attribute.
     */
    @Transient
    @JsonProperty("TRAN_AMT")
    private BigDecimal tranAmt;

    /**
     * Transaction Type Code
     * Note: Transaction posting attribute.
     */
    @Transient
    @JsonProperty("TRAN_TYPE_CD")
    private String tranTypeCd;

    /**
     * Original Transaction Timestamp
     * Note: Transaction posting attribute.
     */
    @Transient
    @JsonProperty("ORIG_TS")
    private String origTs;

}
