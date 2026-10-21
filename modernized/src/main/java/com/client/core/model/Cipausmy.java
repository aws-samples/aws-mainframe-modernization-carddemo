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
 * Modernized JPA Entity generated from Copybook: CIPAUSMY
 * @citation app/app-authorization-ims-db2-mq/cpy/CIPAUSMY.cpy
 * 16 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "cipausmy")
public class Cipausmy implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: PA-ACCT-ID S9(11) (line 19)
     * PIC clause: S9(11)
     */
    @Id
    @Column(name = "pa_acct_id", nullable = false)
    @JsonProperty("PA_ACCT_ID")
    private Long paAcctId;

    /**
     * Original: PA-CUST-ID 9(09) (line 20)
     * PIC clause: 9(09)
     */
    @Column(name = "pa_cust_id")
    @JsonProperty("PA_CUST_ID")
    private Integer paCustId;

    /**
     * Original: PA-AUTH-STATUS X(01) (line 21)
     * PIC clause: X(01)
     */
    @Column(name = "pa_auth_status")
    @JsonProperty("PA_AUTH_STATUS")
    private String paAuthStatus;

    /**
     * Original: PA-ACCOUNT-STATUS X(02) (line 22)
     * PIC clause: X(02)
     */
    @Column(name = "pa_account_status")
    @JsonProperty("PA_ACCOUNT_STATUS")
    private String paAccountStatus;

    /**
     * Original: PA-ACCOUNT-STATUS X(02) occurrence 2 (line 22)
     * PIC clause: X(02)
     */
    @Column(name = "pa_account_status_2")
    @JsonProperty("PA_ACCOUNT_STATUS_2")
    private String paAccountStatus2;

    /**
     * Original: PA-ACCOUNT-STATUS X(02) occurrence 3 (line 22)
     * PIC clause: X(02)
     */
    @Column(name = "pa_account_status_3")
    @JsonProperty("PA_ACCOUNT_STATUS_3")
    private String paAccountStatus3;

    /**
     * Original: PA-ACCOUNT-STATUS X(02) occurrence 4 (line 22)
     * PIC clause: X(02)
     */
    @Column(name = "pa_account_status_4")
    @JsonProperty("PA_ACCOUNT_STATUS_4")
    private String paAccountStatus4;

    /**
     * Original: PA-ACCOUNT-STATUS X(02) occurrence 5 (line 22)
     * PIC clause: X(02)
     */
    @Column(name = "pa_account_status_5")
    @JsonProperty("PA_ACCOUNT_STATUS_5")
    private String paAccountStatus5;

    /**
     * Original: PA-CREDIT-LIMIT S9(09)V99 (line 23)
     * PIC clause: S9(09)V99
     */
    @Column(name = "pa_credit_limit")
    @JsonProperty("PA_CREDIT_LIMIT")
    private BigDecimal paCreditLimit;

    /**
     * Original: PA-CASH-LIMIT S9(09)V99 (line 24)
     * PIC clause: S9(09)V99
     */
    @Column(name = "pa_cash_limit")
    @JsonProperty("PA_CASH_LIMIT")
    private BigDecimal paCashLimit;

    /**
     * Original: PA-CREDIT-BALANCE S9(09)V99 (line 25)
     * PIC clause: S9(09)V99
     */
    @Column(name = "pa_credit_balance")
    @JsonProperty("PA_CREDIT_BALANCE")
    private BigDecimal paCreditBalance;

    /**
     * Original: PA-CASH-BALANCE S9(09)V99 (line 26)
     * PIC clause: S9(09)V99
     */
    @Column(name = "pa_cash_balance")
    @JsonProperty("PA_CASH_BALANCE")
    private BigDecimal paCashBalance;

    /**
     * Original: PA-APPROVED-AUTH-CNT S9(04) (line 27)
     * PIC clause: S9(04)
     */
    @Column(name = "pa_approved_auth_cnt")
    @JsonProperty("PA_APPROVED_AUTH_CNT")
    private Integer paApprovedAuthCnt;

    /**
     * Original: PA-DECLINED-AUTH-CNT S9(04) (line 28)
     * PIC clause: S9(04)
     */
    @Column(name = "pa_declined_auth_cnt")
    @JsonProperty("PA_DECLINED_AUTH_CNT")
    private Integer paDeclinedAuthCnt;

    /**
     * Original: PA-APPROVED-AUTH-AMT S9(09)V99 (line 29)
     * PIC clause: S9(09)V99
     */
    @Column(name = "pa_approved_auth_amt")
    @JsonProperty("PA_APPROVED_AUTH_AMT")
    private BigDecimal paApprovedAuthAmt;

    /**
     * Original: PA-DECLINED-AUTH-AMT S9(09)V99 (line 30)
     * PIC clause: S9(09)V99
     */
    @Column(name = "pa_declined_auth_amt")
    @JsonProperty("PA_DECLINED_AUTH_AMT")
    private BigDecimal paDeclinedAuthAmt;

}
