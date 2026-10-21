package com.carddemo.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.Setter;

import java.math.BigDecimal;

/** CVACT01Y.cpy — ACCOUNT-RECORD (RECLN 300). */
@Entity
@Table(name = "account")
@Getter
@Setter
public class Account {

    /** ACCT-ID PIC 9(11). */
    @Id
    @Column(name = "acct_id")
    private Long id;

    /** ACCT-ACTIVE-STATUS PIC X(01). */
    @Column(name = "active_status", length = 1)
    private String activeStatus;

    /** ACCT-CURR-BAL PIC S9(10)V99. */
    @Column(name = "curr_bal", precision = 12, scale = 2)
    private BigDecimal currBal = BigDecimal.ZERO.setScale(2);

    /** ACCT-CREDIT-LIMIT PIC S9(10)V99. */
    @Column(name = "credit_limit", precision = 12, scale = 2)
    private BigDecimal creditLimit = BigDecimal.ZERO.setScale(2);

    /** ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99. */
    @Column(name = "cash_credit_limit", precision = 12, scale = 2)
    private BigDecimal cashCreditLimit = BigDecimal.ZERO.setScale(2);

    /** ACCT-OPEN-DATE PIC X(10) (YYYY-MM-DD). */
    @Column(name = "open_date", length = 10)
    private String openDate;

    /** ACCT-EXPIRAION-DATE PIC X(10). */
    @Column(name = "expiration_date", length = 10)
    private String expirationDate;

    /** ACCT-REISSUE-DATE PIC X(10). */
    @Column(name = "reissue_date", length = 10)
    private String reissueDate;

    /** ACCT-CURR-CYC-CREDIT PIC S9(10)V99. */
    @Column(name = "curr_cyc_credit", precision = 12, scale = 2)
    private BigDecimal currCycCredit = BigDecimal.ZERO.setScale(2);

    /** ACCT-CURR-CYC-DEBIT PIC S9(10)V99. */
    @Column(name = "curr_cyc_debit", precision = 12, scale = 2)
    private BigDecimal currCycDebit = BigDecimal.ZERO.setScale(2);

    /** ACCT-ADDR-ZIP PIC X(10). */
    @Column(name = "addr_zip", length = 10)
    private String addrZip;

    /** ACCT-GROUP-ID PIC X(10). */
    @Column(name = "group_id", length = 10)
    private String groupId;
}
