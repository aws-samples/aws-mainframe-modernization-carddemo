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
 * Modernized JPA Entity generated from Copybook: CVTRA07Y
 * @citation app/cpy/CVTRA07Y.cpy
 * 17 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "cvtra07y")
public class Cvtra07y implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: REPT-SHORT-NAME X(38) (line 5)
     * PIC clause: X(38)
     */
    @Column(name = "rept_short_name")
    @JsonProperty("REPT_SHORT_NAME")
    private String reptShortName;

    /**
     * Original: REPT-LONG-NAME X(41) (line 7)
     * PIC clause: X(41)
     */
    @Column(name = "rept_long_name")
    @JsonProperty("REPT_LONG_NAME")
    private String reptLongName;

    /**
     * Original: REPT-DATE-HEADER X(12) (line 9)
     * PIC clause: X(12)
     */
    @Column(name = "rept_date_header")
    @JsonProperty("REPT_DATE_HEADER")
    private String reptDateHeader;

    /**
     * Original: REPT-START-DATE X(10) (line 11)
     * PIC clause: X(10)
     */
    @Column(name = "rept_start_date")
    @JsonProperty("REPT_START_DATE")
    private String reptStartDate;

    /**
     * Original: REPT-END-DATE X(10) (line 13)
     * PIC clause: X(10)
     */
    @Column(name = "rept_end_date")
    @JsonProperty("REPT_END_DATE")
    private String reptEndDate;

    /**
     * Original: TRAN-REPORT-TRANS-ID X(16) (line 16)
     * PIC clause: X(16)
     */
    @Id
    @Column(name = "tran_report_trans_id", nullable = false)
    @JsonProperty("TRAN_REPORT_TRANS_ID")
    private String tranReportTransId;

    /**
     * Original: TRAN-REPORT-ACCOUNT-ID X(11) (line 18)
     * PIC clause: X(11)
     */
    @Column(name = "tran_report_account_id")
    @JsonProperty("TRAN_REPORT_ACCOUNT_ID")
    private String tranReportAccountId;

    /**
     * Original: TRAN-REPORT-TYPE-CD X(02) (line 20)
     * PIC clause: X(02)
     */
    @Column(name = "tran_report_type_cd")
    @JsonProperty("TRAN_REPORT_TYPE_CD")
    private String tranReportTypeCd;

    /**
     * Original: TRAN-REPORT-TYPE-DESC X(15) (line 22)
     * PIC clause: X(15)
     */
    @Column(name = "tran_report_type_desc")
    @JsonProperty("TRAN_REPORT_TYPE_DESC")
    private String tranReportTypeDesc;

    /**
     * Original: TRAN-REPORT-CAT-CD 9(04) (line 24)
     * PIC clause: 9(04)
     */
    @Column(name = "tran_report_cat_cd")
    @JsonProperty("TRAN_REPORT_CAT_CD")
    private Integer tranReportCatCd;

    /**
     * Original: TRAN-REPORT-CAT-DESC X(29) (line 26)
     * PIC clause: X(29)
     */
    @Column(name = "tran_report_cat_desc")
    @JsonProperty("TRAN_REPORT_CAT_DESC")
    private String tranReportCatDesc;

    /**
     * Original: TRAN-REPORT-SOURCE X(10) (line 28)
     * PIC clause: X(10)
     */
    @Column(name = "tran_report_source")
    @JsonProperty("TRAN_REPORT_SOURCE")
    private String tranReportSource;

    /**
     * Original: TRAN-REPORT-AMT -ZZZ,ZZZ,ZZZ (line 30)
     * PIC clause: -ZZZ,ZZZ,ZZZ
     */
    @Column(name = "tran_report_amt")
    @JsonProperty("TRAN_REPORT_AMT")
    private String tranReportAmt;

    /**
     * Original: TRANSACTION-HEADER-2 X(133) (line 48)
     * PIC clause: X(133)
     */
    @Column(name = "transaction_header_2")
    @JsonProperty("TRANSACTION_HEADER_2")
    private String transactionHeader2;

    /**
     * Original: REPT-PAGE-TOTAL +ZZZ,ZZZ,ZZZ (line 54)
     * PIC clause: +ZZZ,ZZZ,ZZZ
     */
    @Column(name = "rept_page_total")
    @JsonProperty("REPT_PAGE_TOTAL")
    private String reptPageTotal;

    /**
     * Original: REPT-ACCOUNT-TOTAL +ZZZ,ZZZ,ZZZ (line 60)
     * PIC clause: +ZZZ,ZZZ,ZZZ
     */
    @Column(name = "rept_account_total")
    @JsonProperty("REPT_ACCOUNT_TOTAL")
    private String reptAccountTotal;

    /**
     * Original: REPT-GRAND-TOTAL +ZZZ,ZZZ,ZZZ (line 66)
     * PIC clause: +ZZZ,ZZZ,ZZZ
     */
    @Column(name = "rept_grand_total")
    @JsonProperty("REPT_GRAND_TOTAL")
    private String reptGrandTotal;

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
