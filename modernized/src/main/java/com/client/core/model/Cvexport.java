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
 * Modernized JPA Entity generated from Copybook: CVEXPORT
 * @citation app/cpy/CVEXPORT.cpy
 * 61 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "cvexport")
public class Cvexport implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: EXPORT-REC-TYPE X(1) (line 10)
     * PIC clause: X(1)
     */
    @Column(name = "export_rec_type")
    @JsonProperty("EXPORT_REC_TYPE")
    private String exportRecType;

    /**
     * Original: EXPORT-TIMESTAMP X(26) (line 11)
     * PIC clause: X(26)
     */
    @Column(name = "export_timestamp")
    @JsonProperty("EXPORT_TIMESTAMP")
    private String exportTimestamp;

    /**
     * Original: EXPORT-DATE X(10) (line 13)
     * PIC clause: X(10)
     */
    @Column(name = "export_date")
    @JsonProperty("EXPORT_DATE")
    private String exportDate;

    /**
     * Original: EXPORT-DATE-TIME-SEP X(1) (line 14)
     * PIC clause: X(1)
     */
    @Column(name = "export_date_time_sep")
    @JsonProperty("EXPORT_DATE_TIME_SEP")
    private String exportDateTimeSep;

    /**
     * Original: EXPORT-TIME X(15) (line 15)
     * PIC clause: X(15)
     */
    @Column(name = "export_time")
    @JsonProperty("EXPORT_TIME")
    private String exportTime;

    /**
     * Original: EXPORT-SEQUENCE-NUM 9(9) (line 16)
     * PIC clause: 9(9)
     */
    @Id
    @Column(name = "export_sequence_num", nullable = false)
    @JsonProperty("EXPORT_SEQUENCE_NUM")
    private Integer exportSequenceNum;

    /**
     * Original: EXPORT-BRANCH-ID X(4) (line 17)
     * PIC clause: X(4)
     */
    @Column(name = "export_branch_id")
    @JsonProperty("EXPORT_BRANCH_ID")
    private String exportBranchId;

    /**
     * Original: EXPORT-REGION-CODE X(5) (line 18)
     * PIC clause: X(5)
     */
    @Column(name = "export_region_code")
    @JsonProperty("EXPORT_REGION_CODE")
    private String exportRegionCode;

    /**
     * Original: EXPORT-RECORD-DATA X(460) (line 19)
     * PIC clause: X(460)
     */
    @Column(name = "export_record_data")
    @JsonProperty("EXPORT_RECORD_DATA")
    private String exportRecordData;

    /**
     * Original: EXP-CUST-ID 9(09) (line 25)
     * PIC clause: 9(09)
     */
    @Column(name = "exp_cust_id")
    @JsonProperty("EXP_CUST_ID")
    private Integer expCustId;

    /**
     * Original: EXP-CUST-FIRST-NAME X(25) (line 26)
     * PIC clause: X(25)
     */
    @Column(name = "exp_cust_first_name")
    @JsonProperty("EXP_CUST_FIRST_NAME")
    private String expCustFirstName;

    /**
     * Original: EXP-CUST-MIDDLE-NAME X(25) (line 27)
     * PIC clause: X(25)
     */
    @Column(name = "exp_cust_middle_name")
    @JsonProperty("EXP_CUST_MIDDLE_NAME")
    private String expCustMiddleName;

    /**
     * Original: EXP-CUST-LAST-NAME X(25) (line 28)
     * PIC clause: X(25)
     */
    @Column(name = "exp_cust_last_name")
    @JsonProperty("EXP_CUST_LAST_NAME")
    private String expCustLastName;

    /**
     * Original: EXP-CUST-ADDR-LINE X(50) (line 30)
     * PIC clause: X(50)
     */
    @Column(name = "exp_cust_addr_line")
    @JsonProperty("EXP_CUST_ADDR_LINE")
    private String expCustAddrLine;

    /**
     * Original: EXP-CUST-ADDR-LINE X(50) occurrence 2 (line 30)
     * PIC clause: X(50)
     */
    @Column(name = "exp_cust_addr_line_2")
    @JsonProperty("EXP_CUST_ADDR_LINE_2")
    private String expCustAddrLine2;

    /**
     * Original: EXP-CUST-ADDR-LINE X(50) occurrence 3 (line 30)
     * PIC clause: X(50)
     */
    @Column(name = "exp_cust_addr_line_3")
    @JsonProperty("EXP_CUST_ADDR_LINE_3")
    private String expCustAddrLine3;

    /**
     * Original: EXP-CUST-ADDR-STATE-CD X(02) (line 31)
     * PIC clause: X(02)
     */
    @Column(name = "exp_cust_addr_state_cd")
    @JsonProperty("EXP_CUST_ADDR_STATE_CD")
    private String expCustAddrStateCd;

    /**
     * Original: EXP-CUST-ADDR-COUNTRY-CD X(03) (line 32)
     * PIC clause: X(03)
     */
    @Column(name = "exp_cust_addr_country_cd")
    @JsonProperty("EXP_CUST_ADDR_COUNTRY_CD")
    private String expCustAddrCountryCd;

    /**
     * Original: EXP-CUST-ADDR-ZIP X(10) (line 33)
     * PIC clause: X(10)
     */
    @Column(name = "exp_cust_addr_zip")
    @JsonProperty("EXP_CUST_ADDR_ZIP")
    private String expCustAddrZip;

    /**
     * Original: EXP-CUST-PHONE-NUM X(15) (line 35)
     * PIC clause: X(15)
     */
    @Column(name = "exp_cust_phone_num")
    @JsonProperty("EXP_CUST_PHONE_NUM")
    private String expCustPhoneNum;

    /**
     * Original: EXP-CUST-PHONE-NUM X(15) occurrence 2 (line 35)
     * PIC clause: X(15)
     */
    @Column(name = "exp_cust_phone_num_2")
    @JsonProperty("EXP_CUST_PHONE_NUM_2")
    private String expCustPhoneNum2;

    /**
     * Original: EXP-CUST-SSN 9(09) (line 36)
     * PIC clause: 9(09)
     */
    @Column(name = "exp_cust_ssn")
    @JsonProperty("EXP_CUST_SSN")
    private Integer expCustSsn;

    /**
     * Original: EXP-CUST-GOVT-ISSUED-ID X(20) (line 37)
     * PIC clause: X(20)
     */
    @Column(name = "exp_cust_govt_issued_id")
    @JsonProperty("EXP_CUST_GOVT_ISSUED_ID")
    private String expCustGovtIssuedId;

    /**
     * Original: EXP-CUST-DOB-YYYY-MM-DD X(10) (line 38)
     * PIC clause: X(10)
     */
    @Column(name = "exp_cust_dob_yyyy_mm_dd")
    @JsonProperty("EXP_CUST_DOB_YYYY_MM_DD")
    private String expCustDobYyyyMmDd;

    /**
     * Original: EXP-CUST-EFT-ACCOUNT-ID X(10) (line 39)
     * PIC clause: X(10)
     */
    @Column(name = "exp_cust_eft_account_id")
    @JsonProperty("EXP_CUST_EFT_ACCOUNT_ID")
    private String expCustEftAccountId;

    /**
     * Original: EXP-CUST-PRI-CARD-HOLDER-IND X(01) (line 40)
     * PIC clause: X(01)
     */
    @Column(name = "exp_cust_pri_card_holder_ind")
    @JsonProperty("EXP_CUST_PRI_CARD_HOLDER_IND")
    private String expCustPriCardHolderInd;

    /**
     * Original: EXP-CUST-FICO-CREDIT-SCORE 9(03) (line 41)
     * PIC clause: 9(03)
     */
    @Column(name = "exp_cust_fico_credit_score")
    @JsonProperty("EXP_CUST_FICO_CREDIT_SCORE")
    private Integer expCustFicoCreditScore;

    /**
     * Original: EXP-ACCT-ID 9(11) (line 48)
     * PIC clause: 9(11)
     */
    @Column(name = "exp_acct_id")
    @JsonProperty("EXP_ACCT_ID")
    private Long expAcctId;

    /**
     * Original: EXP-ACCT-ACTIVE-STATUS X(01) (line 49)
     * PIC clause: X(01)
     */
    @Column(name = "exp_acct_active_status")
    @JsonProperty("EXP_ACCT_ACTIVE_STATUS")
    private String expAcctActiveStatus;

    /**
     * Original: EXP-ACCT-CURR-BAL S9(10)V99 (line 50)
     * PIC clause: S9(10)V99
     */
    @Column(name = "exp_acct_curr_bal")
    @JsonProperty("EXP_ACCT_CURR_BAL")
    private BigDecimal expAcctCurrBal;

    /**
     * Original: EXP-ACCT-CREDIT-LIMIT S9(10)V99 (line 51)
     * PIC clause: S9(10)V99
     */
    @Column(name = "exp_acct_credit_limit")
    @JsonProperty("EXP_ACCT_CREDIT_LIMIT")
    private BigDecimal expAcctCreditLimit;

    /**
     * Original: EXP-ACCT-CASH-CREDIT-LIMIT S9(10)V99 (line 52)
     * PIC clause: S9(10)V99
     */
    @Column(name = "exp_acct_cash_credit_limit")
    @JsonProperty("EXP_ACCT_CASH_CREDIT_LIMIT")
    private BigDecimal expAcctCashCreditLimit;

    /**
     * Original: EXP-ACCT-OPEN-DATE X(10) (line 53)
     * PIC clause: X(10)
     */
    @Column(name = "exp_acct_open_date")
    @JsonProperty("EXP_ACCT_OPEN_DATE")
    private String expAcctOpenDate;

    /**
     * Original: EXP-ACCT-EXPIRAION-DATE X(10) (line 54)
     * PIC clause: X(10)
     */
    @Column(name = "exp_acct_expiraion_date")
    @JsonProperty("EXP_ACCT_EXPIRAION_DATE")
    private String expAcctExpiraionDate;

    /**
     * Original: EXP-ACCT-REISSUE-DATE X(10) (line 55)
     * PIC clause: X(10)
     */
    @Column(name = "exp_acct_reissue_date")
    @JsonProperty("EXP_ACCT_REISSUE_DATE")
    private String expAcctReissueDate;

    /**
     * Original: EXP-ACCT-CURR-CYC-CREDIT S9(10)V99 (line 56)
     * PIC clause: S9(10)V99
     */
    @Column(name = "exp_acct_curr_cyc_credit")
    @JsonProperty("EXP_ACCT_CURR_CYC_CREDIT")
    private BigDecimal expAcctCurrCycCredit;

    /**
     * Original: EXP-ACCT-CURR-CYC-DEBIT S9(10)V99 (line 57)
     * PIC clause: S9(10)V99
     */
    @Column(name = "exp_acct_curr_cyc_debit")
    @JsonProperty("EXP_ACCT_CURR_CYC_DEBIT")
    private BigDecimal expAcctCurrCycDebit;

    /**
     * Original: EXP-ACCT-ADDR-ZIP X(10) (line 58)
     * PIC clause: X(10)
     */
    @Column(name = "exp_acct_addr_zip")
    @JsonProperty("EXP_ACCT_ADDR_ZIP")
    private String expAcctAddrZip;

    /**
     * Original: EXP-ACCT-GROUP-ID X(10) (line 59)
     * PIC clause: X(10)
     */
    @Column(name = "exp_acct_group_id")
    @JsonProperty("EXP_ACCT_GROUP_ID")
    private String expAcctGroupId;

    /**
     * Original: EXP-TRAN-ID X(16) (line 66)
     * PIC clause: X(16)
     */
    @Column(name = "exp_tran_id")
    @JsonProperty("EXP_TRAN_ID")
    private String expTranId;

    /**
     * Original: EXP-TRAN-TYPE-CD X(02) (line 67)
     * PIC clause: X(02)
     */
    @Column(name = "exp_tran_type_cd")
    @JsonProperty("EXP_TRAN_TYPE_CD")
    private String expTranTypeCd;

    /**
     * Original: EXP-TRAN-CAT-CD 9(04) (line 68)
     * PIC clause: 9(04)
     */
    @Column(name = "exp_tran_cat_cd")
    @JsonProperty("EXP_TRAN_CAT_CD")
    private Integer expTranCatCd;

    /**
     * Original: EXP-TRAN-SOURCE X(10) (line 69)
     * PIC clause: X(10)
     */
    @Column(name = "exp_tran_source")
    @JsonProperty("EXP_TRAN_SOURCE")
    private String expTranSource;

    /**
     * Original: EXP-TRAN-DESC X(100) (line 70)
     * PIC clause: X(100)
     */
    @Column(name = "exp_tran_desc")
    @JsonProperty("EXP_TRAN_DESC")
    private String expTranDesc;

    /**
     * Original: EXP-TRAN-AMT S9(09)V99 (line 71)
     * PIC clause: S9(09)V99
     */
    @Column(name = "exp_tran_amt")
    @JsonProperty("EXP_TRAN_AMT")
    private BigDecimal expTranAmt;

    /**
     * Original: EXP-TRAN-MERCHANT-ID 9(09) (line 72)
     * PIC clause: 9(09)
     */
    @Column(name = "exp_tran_merchant_id")
    @JsonProperty("EXP_TRAN_MERCHANT_ID")
    private Integer expTranMerchantId;

    /**
     * Original: EXP-TRAN-MERCHANT-NAME X(50) (line 73)
     * PIC clause: X(50)
     */
    @Column(name = "exp_tran_merchant_name")
    @JsonProperty("EXP_TRAN_MERCHANT_NAME")
    private String expTranMerchantName;

    /**
     * Original: EXP-TRAN-MERCHANT-CITY X(50) (line 74)
     * PIC clause: X(50)
     */
    @Column(name = "exp_tran_merchant_city")
    @JsonProperty("EXP_TRAN_MERCHANT_CITY")
    private String expTranMerchantCity;

    /**
     * Original: EXP-TRAN-MERCHANT-ZIP X(10) (line 75)
     * PIC clause: X(10)
     */
    @Column(name = "exp_tran_merchant_zip")
    @JsonProperty("EXP_TRAN_MERCHANT_ZIP")
    private String expTranMerchantZip;

    /**
     * Original: EXP-TRAN-CARD-NUM X(16) (line 76)
     * PIC clause: X(16)
     */
    @Column(name = "exp_tran_card_num")
    @JsonProperty("EXP_TRAN_CARD_NUM")
    private String expTranCardNum;

    /**
     * Original: EXP-TRAN-ORIG-TS X(26) (line 77)
     * PIC clause: X(26)
     */
    @Column(name = "exp_tran_orig_ts")
    @JsonProperty("EXP_TRAN_ORIG_TS")
    private String expTranOrigTs;

    /**
     * Original: EXP-TRAN-PROC-TS X(26) (line 78)
     * PIC clause: X(26)
     */
    @Column(name = "exp_tran_proc_ts")
    @JsonProperty("EXP_TRAN_PROC_TS")
    private String expTranProcTs;

    /**
     * Original: EXP-XREF-CARD-NUM X(16) (line 85)
     * PIC clause: X(16)
     */
    @Column(name = "exp_xref_card_num")
    @JsonProperty("EXP_XREF_CARD_NUM")
    private String expXrefCardNum;

    /**
     * Original: EXP-XREF-CUST-ID 9(09) (line 86)
     * PIC clause: 9(09)
     */
    @Column(name = "exp_xref_cust_id")
    @JsonProperty("EXP_XREF_CUST_ID")
    private Integer expXrefCustId;

    /**
     * Original: EXP-XREF-ACCT-ID 9(11) (line 87)
     * PIC clause: 9(11)
     */
    @Column(name = "exp_xref_acct_id")
    @JsonProperty("EXP_XREF_ACCT_ID")
    private Long expXrefAcctId;

    /**
     * Original: EXP-CARD-NUM X(16) (line 94)
     * PIC clause: X(16)
     */
    @Column(name = "exp_card_num")
    @JsonProperty("EXP_CARD_NUM")
    private String expCardNum;

    /**
     * Original: EXP-CARD-ACCT-ID 9(11) (line 95)
     * PIC clause: 9(11)
     */
    @Column(name = "exp_card_acct_id")
    @JsonProperty("EXP_CARD_ACCT_ID")
    private Long expCardAcctId;

    /**
     * Original: EXP-CARD-CVV-CD 9(03) (line 96)
     * PIC clause: 9(03)
     * Note: PCI-DSS 3.2 Prohibited Storage - Excluded from persistent database columns.
     */
    @Transient
    @JsonProperty("EXP_CARD_CVV_CD")
    private Integer expCardCvvCd;

    /**
     * Original: EXP-CARD-EMBOSSED-NAME X(50) (line 97)
     * PIC clause: X(50)
     */
    @Column(name = "exp_card_embossed_name")
    @JsonProperty("EXP_CARD_EMBOSSED_NAME")
    private String expCardEmbossedName;

    /**
     * Original: EXP-CARD-EXPIRAION-DATE X(10) (line 98)
     * PIC clause: X(10)
     */
    @Column(name = "exp_card_expiraion_date")
    @JsonProperty("EXP_CARD_EXPIRAION_DATE")
    private String expCardExpiraionDate;

    /**
     * Original: EXP-CARD-ACTIVE-STATUS X(01) (line 99)
     * PIC clause: X(01)
     */
    @Column(name = "exp_card_active_status")
    @JsonProperty("EXP_CARD_ACTIVE_STATUS")
    private String expCardActiveStatus;

}
