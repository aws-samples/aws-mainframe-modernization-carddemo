package com.carddemo.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Version;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

import java.time.LocalDate;

/** CARDDAT (CVACT02Y). CVV is not persisted (plan R5). */
@Entity
@Table(name = "cards")
public class Card {

    @Id
    @JdbcTypeCode(SqlTypes.CHAR)
    @Column(name = "card_number")
    private String cardNumber;

    @Column(name = "account_id")
    private Long accountId;

    @Column(name = "embossed_name")
    private String embossedName;

    @Column(name = "expiration_date")
    private LocalDate expirationDate;

    @JdbcTypeCode(SqlTypes.CHAR)
    @Column(name = "active_status")
    private String activeStatus;

    @Version
    private long version;

    public String getCardNumber() {
        return cardNumber;
    }

    public Long getAccountId() {
        return accountId;
    }

    public String getEmbossedName() {
        return embossedName;
    }

    public LocalDate getExpirationDate() {
        return expirationDate;
    }

    public String getActiveStatus() {
        return activeStatus;
    }

    public long getVersion() {
        return version;
    }

    // -- mutators for the COCRDUPC update path (Phase 2, plan §4) ----------

    public void setEmbossedName(String embossedName) {
        this.embossedName = embossedName;
    }

    public void setExpirationDate(LocalDate expirationDate) {
        this.expirationDate = expirationDate;
    }

    public void setActiveStatus(String activeStatus) {
        this.activeStatus = activeStatus;
    }

    /** {@code ****-****-****-1234} — list-view masking (plan §2.4). */
    public String maskedNumber() {
        String n = cardNumber == null ? "" : cardNumber.trim();
        String last4 = n.length() >= 4 ? n.substring(n.length() - 4) : n;
        return "****-****-****-" + last4;
    }
}
