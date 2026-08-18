package com.carddemo.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/** TRANSACT (CVTRA05Y). Read-only in Phase 1; inserted by Phase 3 (COTRN02C add / COBIL00C bill pay). */
@Entity
@Table(name = "transactions")
public class Transaction {

    @Id
    @JdbcTypeCode(SqlTypes.CHAR)
    @Column(name = "transaction_id")
    private String transactionId;

    @JdbcTypeCode(SqlTypes.CHAR)
    @Column(name = "type_cd")
    private String typeCd;

    @JdbcTypeCode(SqlTypes.SMALLINT)
    @Column(name = "category_cd")
    private Integer categoryCd;

    @Column(name = "source")
    private String source;

    @Column(name = "description")
    private String description;

    @Column(name = "amount")
    private BigDecimal amount;

    @Column(name = "merchant_id")
    private Long merchantId;

    @Column(name = "merchant_name")
    private String merchantName;

    @Column(name = "merchant_city")
    private String merchantCity;

    @Column(name = "merchant_zip")
    private String merchantZip;

    @JdbcTypeCode(SqlTypes.CHAR)
    @Column(name = "card_number")
    private String cardNumber;

    @Column(name = "orig_ts")
    private LocalDateTime origTs;

    @Column(name = "proc_ts")
    private LocalDateTime procTs;

    public String getTransactionId() {
        return transactionId;
    }

    public String getTypeCd() {
        return typeCd;
    }

    public Integer getCategoryCd() {
        return categoryCd;
    }

    public String getSource() {
        return source;
    }

    public String getDescription() {
        return description;
    }

    public BigDecimal getAmount() {
        return amount;
    }

    public Long getMerchantId() {
        return merchantId;
    }

    public String getMerchantName() {
        return merchantName;
    }

    public String getMerchantCity() {
        return merchantCity;
    }

    public String getMerchantZip() {
        return merchantZip;
    }

    public String getCardNumber() {
        return cardNumber;
    }

    /** {@code ****-****-****-1234} — plan §5 masking outside card detail. */
    public String maskedCardNumber() {
        String n = cardNumber == null ? "" : cardNumber.trim();
        String last4 = n.length() >= 4 ? n.substring(n.length() - 4) : n;
        return "****-****-****-" + last4;
    }

    public LocalDateTime getOrigTs() {
        return origTs;
    }

    public LocalDateTime getProcTs() {
        return procTs;
    }

    // -- mutators for the Phase 3 insert paths (COTRN02C ADD-TRANSACTION /
    //    COBIL00C bill-pay TRAN-RECORD build, plan §4) ----------------------

    public void setTransactionId(String transactionId) {
        this.transactionId = transactionId;
    }

    public void setTypeCd(String typeCd) {
        this.typeCd = typeCd;
    }

    public void setCategoryCd(Integer categoryCd) {
        this.categoryCd = categoryCd;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public void setAmount(BigDecimal amount) {
        this.amount = amount;
    }

    public void setMerchantId(Long merchantId) {
        this.merchantId = merchantId;
    }

    public void setMerchantName(String merchantName) {
        this.merchantName = merchantName;
    }

    public void setMerchantCity(String merchantCity) {
        this.merchantCity = merchantCity;
    }

    public void setMerchantZip(String merchantZip) {
        this.merchantZip = merchantZip;
    }

    public void setCardNumber(String cardNumber) {
        this.cardNumber = cardNumber;
    }

    public void setOrigTs(LocalDateTime origTs) {
        this.origTs = origTs;
    }

    public void setProcTs(LocalDateTime procTs) {
        this.procTs = procTs;
    }
}
