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
 * Modernized JPA Entity generated from Copybook: COSTM01
 * @citation app/cpy/COSTM01.CPY
 * 13 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "costm01")
public class Costm01 implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: TRNX-CARD-NUM X(16) (line 22)
     * PIC clause: X(16)
     */
    @Id
    @Column(name = "trnx_card_num", nullable = false)
    @JsonProperty("TRNX_CARD_NUM")
    private String trnxCardNum;

    /**
     * Original: TRNX-ID X(16) (line 23)
     * PIC clause: X(16)
     */
    @Column(name = "trnx_id")
    @JsonProperty("TRNX_ID")
    private String trnxId;

    /**
     * Original: TRNX-TYPE-CD X(02) (line 25)
     * PIC clause: X(02)
     */
    @Column(name = "trnx_type_cd")
    @JsonProperty("TRNX_TYPE_CD")
    private String trnxTypeCd;

    /**
     * Original: TRNX-CAT-CD 9(04) (line 26)
     * PIC clause: 9(04)
     */
    @Column(name = "trnx_cat_cd")
    @JsonProperty("TRNX_CAT_CD")
    private Integer trnxCatCd;

    /**
     * Original: TRNX-SOURCE X(10) (line 27)
     * PIC clause: X(10)
     */
    @Column(name = "trnx_source")
    @JsonProperty("TRNX_SOURCE")
    private String trnxSource;

    /**
     * Original: TRNX-DESC X(100) (line 28)
     * PIC clause: X(100)
     */
    @Column(name = "trnx_desc")
    @JsonProperty("TRNX_DESC")
    private String trnxDesc;

    /**
     * Original: TRNX-AMT S9(09)V99 (line 29)
     * PIC clause: S9(09)V99
     */
    @Column(name = "trnx_amt")
    @JsonProperty("TRNX_AMT")
    private BigDecimal trnxAmt;

    /**
     * Original: TRNX-MERCHANT-ID 9(09) (line 30)
     * PIC clause: 9(09)
     */
    @Column(name = "trnx_merchant_id")
    @JsonProperty("TRNX_MERCHANT_ID")
    private Integer trnxMerchantId;

    /**
     * Original: TRNX-MERCHANT-NAME X(50) (line 31)
     * PIC clause: X(50)
     */
    @Column(name = "trnx_merchant_name")
    @JsonProperty("TRNX_MERCHANT_NAME")
    private String trnxMerchantName;

    /**
     * Original: TRNX-MERCHANT-CITY X(50) (line 32)
     * PIC clause: X(50)
     */
    @Column(name = "trnx_merchant_city")
    @JsonProperty("TRNX_MERCHANT_CITY")
    private String trnxMerchantCity;

    /**
     * Original: TRNX-MERCHANT-ZIP X(10) (line 33)
     * PIC clause: X(10)
     */
    @Column(name = "trnx_merchant_zip")
    @JsonProperty("TRNX_MERCHANT_ZIP")
    private String trnxMerchantZip;

    /**
     * Original: TRNX-ORIG-TS X(26) (line 34)
     * PIC clause: X(26)
     */
    @Column(name = "trnx_orig_ts")
    @JsonProperty("TRNX_ORIG_TS")
    private String trnxOrigTs;

    /**
     * Original: TRNX-PROC-TS X(26) (line 35)
     * PIC clause: X(26)
     */
    @Column(name = "trnx_proc_ts")
    @JsonProperty("TRNX_PROC_TS")
    private String trnxProcTs;

}
