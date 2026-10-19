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
 * Modernized JPA Entity generated from Copybook: CVTRA05Y
 * @citation app/cpy/CVTRA05Y.cpy
 * 13 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "cvtra05y")
public class Cvtra05y implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: TRAN-ID X(16) (line 5)
     * PIC clause: X(16)
     */
    @Id
    @Column(name = "tran_id", nullable = false)
    @JsonProperty("TRAN_ID")
    private String tranId;

    /**
     * Original: TRAN-TYPE-CD X(02) (line 6)
     * PIC clause: X(02)
     */
    @Column(name = "tran_type_cd")
    @JsonProperty("TRAN_TYPE_CD")
    private String tranTypeCd;

    /**
     * Original: TRAN-CAT-CD 9(04) (line 7)
     * PIC clause: 9(04)
     */
    @Column(name = "tran_cat_cd")
    @JsonProperty("TRAN_CAT_CD")
    private Integer tranCatCd;

    /**
     * Original: TRAN-SOURCE X(10) (line 8)
     * PIC clause: X(10)
     */
    @Column(name = "tran_source")
    @JsonProperty("TRAN_SOURCE")
    private String tranSource;

    /**
     * Original: TRAN-DESC X(100) (line 9)
     * PIC clause: X(100)
     */
    @Column(name = "tran_desc")
    @JsonProperty("TRAN_DESC")
    private String tranDesc;

    /**
     * Original: TRAN-AMT S9(09)V99 (line 10)
     * PIC clause: S9(09)V99
     */
    @Column(name = "tran_amt")
    @JsonProperty("TRAN_AMT")
    private BigDecimal tranAmt;

    /**
     * Original: TRAN-MERCHANT-ID 9(09) (line 11)
     * PIC clause: 9(09)
     */
    @Column(name = "tran_merchant_id")
    @JsonProperty("TRAN_MERCHANT_ID")
    private Integer tranMerchantId;

    /**
     * Original: TRAN-MERCHANT-NAME X(50) (line 12)
     * PIC clause: X(50)
     */
    @Column(name = "tran_merchant_name")
    @JsonProperty("TRAN_MERCHANT_NAME")
    private String tranMerchantName;

    /**
     * Original: TRAN-MERCHANT-CITY X(50) (line 13)
     * PIC clause: X(50)
     */
    @Column(name = "tran_merchant_city")
    @JsonProperty("TRAN_MERCHANT_CITY")
    private String tranMerchantCity;

    /**
     * Original: TRAN-MERCHANT-ZIP X(10) (line 14)
     * PIC clause: X(10)
     */
    @Column(name = "tran_merchant_zip")
    @JsonProperty("TRAN_MERCHANT_ZIP")
    private String tranMerchantZip;

    /**
     * Original: TRAN-CARD-NUM X(16) (line 15)
     * PIC clause: X(16)
     */
    @Column(name = "tran_card_num")
    @JsonProperty("TRAN_CARD_NUM")
    private String tranCardNum;

    /**
     * Original: TRAN-ORIG-TS X(26) (line 16)
     * PIC clause: X(26)
     */
    @Column(name = "tran_orig_ts")
    @JsonProperty("TRAN_ORIG_TS")
    private String tranOrigTs;

    /**
     * Original: TRAN-PROC-TS X(26) (line 17)
     * PIC clause: X(26)
     */
    @Column(name = "tran_proc_ts")
    @JsonProperty("TRAN_PROC_TS")
    private String tranProcTs;

    /**
     * Card PAN Used in Transaction
     * Note: Transaction posting attribute.
     */
    @Transient
    @JsonProperty("CARD_NUM")
    private String cardNum;

    /**
     * Original Transaction Timestamp
     * Note: Transaction posting attribute.
     */
    @Transient
    @JsonProperty("ORIG_TS")
    private String origTs;

}
