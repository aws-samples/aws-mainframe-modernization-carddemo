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
 * Modernized JPA Entity generated from Copybook: CVTRA06Y
 * @citation app/cpy/CVTRA06Y.cpy
 * 13 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "cvtra06y")
public class Cvtra06y implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: DALYTRAN-ID X(16) (line 5)
     * PIC clause: X(16)
     */
    @Id
    @Column(name = "dalytran_id", nullable = false)
    @JsonProperty("DALYTRAN_ID")
    private String dalytranId;

    /**
     * Original: DALYTRAN-TYPE-CD X(02) (line 6)
     * PIC clause: X(02)
     */
    @Column(name = "dalytran_type_cd")
    @JsonProperty("DALYTRAN_TYPE_CD")
    private String dalytranTypeCd;

    /**
     * Original: DALYTRAN-CAT-CD 9(04) (line 7)
     * PIC clause: 9(04)
     */
    @Column(name = "dalytran_cat_cd")
    @JsonProperty("DALYTRAN_CAT_CD")
    private Integer dalytranCatCd;

    /**
     * Original: DALYTRAN-SOURCE X(10) (line 8)
     * PIC clause: X(10)
     */
    @Column(name = "dalytran_source")
    @JsonProperty("DALYTRAN_SOURCE")
    private String dalytranSource;

    /**
     * Original: DALYTRAN-DESC X(100) (line 9)
     * PIC clause: X(100)
     */
    @Column(name = "dalytran_desc")
    @JsonProperty("DALYTRAN_DESC")
    private String dalytranDesc;

    /**
     * Original: DALYTRAN-AMT S9(09)V99 (line 10)
     * PIC clause: S9(09)V99
     */
    @Column(name = "dalytran_amt")
    @JsonProperty("DALYTRAN_AMT")
    private BigDecimal dalytranAmt;

    /**
     * Original: DALYTRAN-MERCHANT-ID 9(09) (line 11)
     * PIC clause: 9(09)
     */
    @Column(name = "dalytran_merchant_id")
    @JsonProperty("DALYTRAN_MERCHANT_ID")
    private Integer dalytranMerchantId;

    /**
     * Original: DALYTRAN-MERCHANT-NAME X(50) (line 12)
     * PIC clause: X(50)
     */
    @Column(name = "dalytran_merchant_name")
    @JsonProperty("DALYTRAN_MERCHANT_NAME")
    private String dalytranMerchantName;

    /**
     * Original: DALYTRAN-MERCHANT-CITY X(50) (line 13)
     * PIC clause: X(50)
     */
    @Column(name = "dalytran_merchant_city")
    @JsonProperty("DALYTRAN_MERCHANT_CITY")
    private String dalytranMerchantCity;

    /**
     * Original: DALYTRAN-MERCHANT-ZIP X(10) (line 14)
     * PIC clause: X(10)
     */
    @Column(name = "dalytran_merchant_zip")
    @JsonProperty("DALYTRAN_MERCHANT_ZIP")
    private String dalytranMerchantZip;

    /**
     * Original: DALYTRAN-CARD-NUM X(16) (line 15)
     * PIC clause: X(16)
     */
    @Column(name = "dalytran_card_num")
    @JsonProperty("DALYTRAN_CARD_NUM")
    private String dalytranCardNum;

    /**
     * Original: DALYTRAN-ORIG-TS X(26) (line 16)
     * PIC clause: X(26)
     */
    @Column(name = "dalytran_orig_ts")
    @JsonProperty("DALYTRAN_ORIG_TS")
    private String dalytranOrigTs;

    /**
     * Original: DALYTRAN-PROC-TS X(26) (line 17)
     * PIC clause: X(26)
     */
    @Column(name = "dalytran_proc_ts")
    @JsonProperty("DALYTRAN_PROC_TS")
    private String dalytranProcTs;

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
