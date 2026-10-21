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
 * Modernized JPA Entity generated from Copybook: CVACT01Y
 * @citation cpy/CVACT01Y.cpy
 * 12 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "cvact01y")
public class Cvact01y implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: ACCT-ID 9(11) (line 5)
     * PIC clause: 9(11)
     */
    @Id
    @Column(name = "acct_id", nullable = false)
    @JsonProperty("ACCT_ID")
    private Long acctId;

    /**
     * Original: ACCT-ACTIVE-STATUS X(01) (line 6)
     * PIC clause: X(01)
     */
    @Column(name = "acct_active_status")
    @JsonProperty("ACCT_ACTIVE_STATUS")
    private String acctActiveStatus;

    /**
     * Original: ACCT-CURR-BAL S9(10)V99 (line 7)
     * PIC clause: S9(10)V99
     */
    @Column(name = "acct_curr_bal")
    @JsonProperty("ACCT_CURR_BAL")
    private BigDecimal acctCurrBal;

    /**
     * Original: ACCT-CREDIT-LIMIT S9(10)V99 (line 8)
     * PIC clause: S9(10)V99
     */
    @Column(name = "acct_credit_limit")
    @JsonProperty("ACCT_CREDIT_LIMIT")
    private BigDecimal acctCreditLimit;

    /**
     * Original: ACCT-CASH-CREDIT-LIMIT S9(10)V99 (line 9)
     * PIC clause: S9(10)V99
     */
    @Column(name = "acct_cash_credit_limit")
    @JsonProperty("ACCT_CASH_CREDIT_LIMIT")
    private BigDecimal acctCashCreditLimit;

    /**
     * Original: ACCT-OPEN-DATE X(10) (line 10)
     * PIC clause: X(10)
     */
    @Column(name = "acct_open_date")
    @JsonProperty("ACCT_OPEN_DATE")
    private String acctOpenDate;

    /**
     * Original: ACCT-EXPIRAION-DATE X(10) (line 11)
     * PIC clause: X(10)
     */
    @Column(name = "acct_expiraion_date")
    @JsonProperty("ACCT_EXPIRAION_DATE")
    private String acctExpiraionDate;

    /**
     * Original: ACCT-REISSUE-DATE X(10) (line 12)
     * PIC clause: X(10)
     */
    @Column(name = "acct_reissue_date")
    @JsonProperty("ACCT_REISSUE_DATE")
    private String acctReissueDate;

    /**
     * Original: ACCT-CURR-CYC-CREDIT S9(10)V99 (line 13)
     * PIC clause: S9(10)V99
     */
    @Column(name = "acct_curr_cyc_credit")
    @JsonProperty("ACCT_CURR_CYC_CREDIT")
    private BigDecimal acctCurrCycCredit;

    /**
     * Original: ACCT-CURR-CYC-DEBIT S9(10)V99 (line 14)
     * PIC clause: S9(10)V99
     */
    @Column(name = "acct_curr_cyc_debit")
    @JsonProperty("ACCT_CURR_CYC_DEBIT")
    private BigDecimal acctCurrCycDebit;

    /**
     * Original: ACCT-ADDR-ZIP X(10) (line 15)
     * PIC clause: X(10)
     */
    @Column(name = "acct_addr_zip")
    @JsonProperty("ACCT_ADDR_ZIP")
    private String acctAddrZip;

    /**
     * Original: ACCT-GROUP-ID X(10) (line 16)
     * PIC clause: X(10)
     */
    @Column(name = "acct_group_id")
    @JsonProperty("ACCT_GROUP_ID")
    private String acctGroupId;

    public String getAcctExpDate() {
        return this.acctExpiraionDate != null ? this.acctExpiraionDate : "";
    }

    public void setAcctExpDate(String acctExpDate) {
        this.acctExpiraionDate = acctExpDate;
    }

}
