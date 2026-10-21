package com.carddemo.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.Setter;

/** CVACT02Y.cpy — CARD-RECORD (RECLN 150). */
@Entity
@Table(name = "card")
@Getter
@Setter
public class Card {

    /** CARD-NUM PIC X(16). */
    @Id
    @Column(name = "card_num", length = 16)
    private String cardNum;

    /** CARD-ACCT-ID PIC 9(11). */
    @Column(name = "acct_id")
    private Long acctId;

    /** CARD-CVV-CD PIC 9(03). */
    @Column(name = "cvv_cd")
    private Integer cvvCd;

    /** CARD-EMBOSSED-NAME PIC X(50). */
    @Column(name = "embossed_name", length = 50)
    private String embossedName;

    /** CARD-EXPIRAION-DATE PIC X(10). */
    @Column(name = "expiration_date", length = 10)
    private String expirationDate;

    /** CARD-ACTIVE-STATUS PIC X(01). */
    @Column(name = "active_status", length = 1)
    private String activeStatus;
}
