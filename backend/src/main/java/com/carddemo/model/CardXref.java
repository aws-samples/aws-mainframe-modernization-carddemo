package com.carddemo.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.Setter;

/** CVACT03Y.cpy — CARD-XREF-RECORD (RECLN 50). */
@Entity
@Table(name = "card_xref")
@Getter
@Setter
public class CardXref {

    /** XREF-CARD-NUM PIC X(16). */
    @Id
    @Column(name = "card_num", length = 16)
    private String cardNum;

    /** XREF-CUST-ID PIC 9(09). */
    @Column(name = "cust_id")
    private Long custId;

    /** XREF-ACCT-ID PIC 9(11). */
    @Column(name = "acct_id")
    private Long acctId;
}
