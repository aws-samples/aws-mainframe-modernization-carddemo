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
 * Modernized JPA Entity generated from Copybook: FILE_TRANSACT_FILE
 * @citation FILE_TRANSACT_FILE (source not found)
 * 13 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "file_transact_file")
public class FileTransactFile implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Unique Transaction Identifier
     * PIC clause: X(16)
     */
    @Id
    @Column(name = "tran_id", nullable = false)
    @JsonProperty("TRAN_ID")
    private String tranId;

    /**
     * Card PAN Used in Transaction
     * PIC clause: X(16)
     */
    @Column(name = "card_num")
    @JsonProperty("CARD_NUM")
    private String cardNum;

    /**
     * Transaction Type Code (01=Purchase, 02=Cash Advance)
     * PIC clause: X(02)
     */
    @Column(name = "tran_type_cd")
    @JsonProperty("TRAN_TYPE_CD")
    private String tranTypeCd;

    /**
     * Transaction Category Code
     * PIC clause: 9(04)
     */
    @Column(name = "tran_cat_cd")
    @JsonProperty("TRAN_CAT_CD")
    private Integer tranCatCd;

    /**
     * Transaction Channel (POS, ATM, ONLINE)
     * PIC clause: X(10)
     */
    @Column(name = "tran_source")
    @JsonProperty("TRAN_SOURCE")
    private String tranSource;

    /**
     * Merchant or Settlement Description
     * PIC clause: X(100)
     */
    @Column(name = "tran_desc")
    @JsonProperty("TRAN_DESC")
    private String tranDesc;

    /**
     * Authorized Transaction Amount
     * PIC clause: S9(09)V99
     */
    @Column(name = "tran_amt")
    @JsonProperty("TRAN_AMT")
    private BigDecimal tranAmt;

    /**
     * Merchant Terminal Identification
     * PIC clause: 9(09)
     */
    @Column(name = "merchant_id")
    @JsonProperty("MERCHANT_ID")
    private Integer merchantId;

    /**
     * Merchant Trading Name
     * PIC clause: X(50)
     */
    @Column(name = "merchant_name")
    @JsonProperty("MERCHANT_NAME")
    private String merchantName;

    /**
     * Merchant Location City
     * PIC clause: X(50)
     */
    @Column(name = "merchant_city")
    @JsonProperty("MERCHANT_CITY")
    private String merchantCity;

    /**
     * Merchant Zip / Postal Code
     * PIC clause: X(10)
     */
    @Column(name = "merchant_zip")
    @JsonProperty("MERCHANT_ZIP")
    private String merchantZip;

    /**
     * Original Transaction Timestamp
     * PIC clause: X(26)
     */
    @Column(name = "orig_ts")
    @JsonProperty("ORIG_TS")
    private String origTs;

    /**
     * Posting / Settlement Timestamp
     * PIC clause: X(26)
     */
    @Column(name = "proc_ts")
    @JsonProperty("PROC_TS")
    private String procTs;

}
