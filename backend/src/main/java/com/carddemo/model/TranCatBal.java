package com.carddemo.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.IdClass;
import jakarta.persistence.Table;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.math.BigDecimal;

/** CVTRA01Y.cpy — TRAN-CAT-BAL-RECORD (RECLN 50). */
@Entity
@Table(name = "tran_cat_bal")
@IdClass(TranCatBal.Key.class)
@Getter
@Setter
public class TranCatBal {

    /** TRANCAT-ACCT-ID PIC 9(11). */
    @Id
    @Column(name = "acct_id")
    private Long acctId;

    /** TRANCAT-TYPE-CD PIC X(02). */
    @Id
    @Column(name = "type_cd", length = 2)
    private String typeCd;

    /** TRANCAT-CD PIC 9(04). */
    @Id
    @Column(name = "cat_cd")
    private Integer catCd;

    /** TRAN-CAT-BAL PIC S9(09)V99. */
    @Column(name = "bal", precision = 11, scale = 2)
    private BigDecimal bal = BigDecimal.ZERO.setScale(2);

    @Getter
    @Setter
    @EqualsAndHashCode
    public static class Key implements Serializable {
        private Long acctId;
        private String typeCd;
        private Integer catCd;

        public Key() {}

        public Key(Long acctId, String typeCd, Integer catCd) {
            this.acctId = acctId;
            this.typeCd = typeCd;
            this.catCd = catCd;
        }
    }
}
