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
 * Modernized JPA Entity generated from Copybook: FILE_ACCOUNT_INPUT
 * @citation FILE_ACCOUNT_INPUT (source not found)
 * 14 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "file_account_input")
public class FileAccountInput implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Primary Account Identification Number
     * PIC clause: 9(11)
     */
    @Id
    @Column(name = "acct_id", nullable = false)
    @JsonProperty("ACCT_ID")
    private Long acctId;

    /**
     * Account Status (Y=Active, N=Suspended)
     * PIC clause: X(01)
     */
    @Column(name = "acct_active_status")
    @JsonProperty("ACCT_ACTIVE_STATUS")
    private String acctActiveStatus;

    /**
     * Current Ledger Balance
     * PIC clause: S9(10)V99
     */
    @Column(name = "acct_curr_bal")
    @JsonProperty("ACCT_CURR_BAL")
    private BigDecimal acctCurrBal;

    /**
     * Maximum Authorized Credit Limit
     * PIC clause: S9(10)V99
     */
    @Column(name = "acct_credit_limit")
    @JsonProperty("ACCT_CREDIT_LIMIT")
    private BigDecimal acctCreditLimit;

    /**
     * Cash Advance Credit Limit
     * PIC clause: S9(10)V99
     */
    @Column(name = "acct_cash_credit_limit")
    @JsonProperty("ACCT_CASH_CREDIT_LIMIT")
    private BigDecimal acctCashCreditLimit;

    /**
     * Account Opening Date (YYYY-MM-DD)
     * PIC clause: X(10)
     */
    @Column(name = "acct_open_date")
    @JsonProperty("ACCT_OPEN_DATE")
    private String acctOpenDate;

    /**
     * Account Expiration Date (YYYY-MM-DD)
     * PIC clause: X(10)
     */
    @Column(name = "acct_exp_date")
    @JsonProperty("ACCT_EXP_DATE")
    private String acctExpDate;

    /**
     * Card Reissue Timestamp
     * PIC clause: X(10)
     */
    @Column(name = "acct_reissue_date")
    @JsonProperty("ACCT_REISSUE_DATE")
    private String acctReissueDate;

    /**
     * Cycle Aggregate Credits
     * PIC clause: S9(10)V99
     */
    @Column(name = "acct_curr_cyc_credit")
    @JsonProperty("ACCT_CURR_CYC_CREDIT")
    private BigDecimal acctCurrCycCredit;

    /**
     * Cycle Aggregate Debits
     * PIC clause: S9(10)V99
     */
    @Column(name = "acct_curr_cyc_debit")
    @JsonProperty("ACCT_CURR_CYC_DEBIT")
    private BigDecimal acctCurrCycDebit;

    /**
     * Billing Zip / Postal Code
     * PIC clause: X(10)
     */
    @Column(name = "acct_addr_zip")
    @JsonProperty("ACCT_ADDR_ZIP")
    private String acctAddrZip;

    /**
     * Product Portfolio Group Code
     * PIC clause: X(10)
     */
    @Column(name = "acct_group_id")
    @JsonProperty("ACCT_GROUP_ID")
    private String acctGroupId;

    /**
     * Associated Primary Customer ID
     * PIC clause: 9(09)
     */
    @Column(name = "cust_id")
    @JsonProperty("CUST_ID")
    private Integer custId;

    /**
     * Card Security Code (CVV)
     * PIC clause: 9(03)
     * Note: PCI-DSS 3.2 Prohibited Storage - Excluded from persistent database columns.
     */
    @Transient
    @JsonProperty("ACCT_CARD_CVV_CD")
    private Integer acctCardCvvCd;

    public String getAcctExpiraionDate() {
        return this.acctExpDate != null ? this.acctExpDate : "";
    }

    public void setAcctExpiraionDate(String acctExpiraionDate) {
        this.acctExpDate = acctExpiraionDate;
    }

}
