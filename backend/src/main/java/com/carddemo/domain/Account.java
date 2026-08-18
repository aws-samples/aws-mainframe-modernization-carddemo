package com.carddemo.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Version;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

import java.math.BigDecimal;
import java.time.LocalDate;

/** ACCTDAT (CVACT01Y). customer_id is the dissolved CXACAIX xref. */
@Entity
@Table(name = "accounts")
public class Account {

    @Id
    @Column(name = "account_id")
    private Long accountId;

    @JdbcTypeCode(SqlTypes.CHAR)
    @Column(name = "active_status")
    private String activeStatus;

    @Column(name = "curr_bal")
    private BigDecimal currBal;

    @Column(name = "credit_limit")
    private BigDecimal creditLimit;

    @Column(name = "cash_credit_limit")
    private BigDecimal cashCreditLimit;

    @Column(name = "open_date")
    private LocalDate openDate;

    @Column(name = "expiration_date")
    private LocalDate expirationDate;

    @Column(name = "reissue_date")
    private LocalDate reissueDate;

    @Column(name = "curr_cyc_credit")
    private BigDecimal currCycCredit;

    @Column(name = "curr_cyc_debit")
    private BigDecimal currCycDebit;

    @Column(name = "zip")
    private String zip;

    @Column(name = "group_id")
    private String groupId;

    @Column(name = "customer_id")
    private Long customerId;

    @Version
    private long version;

    public Long getAccountId() {
        return accountId;
    }

    public String getActiveStatus() {
        return activeStatus;
    }

    public BigDecimal getCurrBal() {
        return currBal;
    }

    public BigDecimal getCreditLimit() {
        return creditLimit;
    }

    public BigDecimal getCashCreditLimit() {
        return cashCreditLimit;
    }

    public LocalDate getOpenDate() {
        return openDate;
    }

    public LocalDate getExpirationDate() {
        return expirationDate;
    }

    public LocalDate getReissueDate() {
        return reissueDate;
    }

    public BigDecimal getCurrCycCredit() {
        return currCycCredit;
    }

    public BigDecimal getCurrCycDebit() {
        return currCycDebit;
    }

    public String getZip() {
        return zip;
    }

    public String getGroupId() {
        return groupId;
    }

    public Long getCustomerId() {
        return customerId;
    }

    public long getVersion() {
        return version;
    }

    // -- mutators for the COACTUPC update path (Phase 2, plan §4) ----------

    public void setActiveStatus(String activeStatus) {
        this.activeStatus = activeStatus;
    }

    public void setCurrBal(BigDecimal currBal) {
        this.currBal = currBal;
    }

    public void setCreditLimit(BigDecimal creditLimit) {
        this.creditLimit = creditLimit;
    }

    public void setCashCreditLimit(BigDecimal cashCreditLimit) {
        this.cashCreditLimit = cashCreditLimit;
    }

    public void setOpenDate(LocalDate openDate) {
        this.openDate = openDate;
    }

    public void setExpirationDate(LocalDate expirationDate) {
        this.expirationDate = expirationDate;
    }

    public void setReissueDate(LocalDate reissueDate) {
        this.reissueDate = reissueDate;
    }

    public void setCurrCycCredit(BigDecimal currCycCredit) {
        this.currCycCredit = currCycCredit;
    }

    public void setCurrCycDebit(BigDecimal currCycDebit) {
        this.currCycDebit = currCycDebit;
    }

    public void setGroupId(String groupId) {
        this.groupId = groupId;
    }
}
