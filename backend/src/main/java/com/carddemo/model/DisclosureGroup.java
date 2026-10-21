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

/** CVTRA02Y.cpy — DIS-GROUP-RECORD (RECLN 50). */
@Entity
@Table(name = "disclosure_group")
@IdClass(DisclosureGroup.Key.class)
@Getter
@Setter
public class DisclosureGroup {

    /** DIS-ACCT-GROUP-ID PIC X(10). */
    @Id
    @Column(name = "acct_group_id", length = 10)
    private String acctGroupId;

    /** DIS-TRAN-TYPE-CD PIC X(02). */
    @Id
    @Column(name = "tran_type_cd", length = 2)
    private String tranTypeCd;

    /** DIS-TRAN-CAT-CD PIC 9(04). */
    @Id
    @Column(name = "tran_cat_cd")
    private Integer tranCatCd;

    /** DIS-INT-RATE PIC S9(04)V99 (annual percentage). */
    @Column(name = "int_rate", precision = 6, scale = 2)
    private BigDecimal intRate = BigDecimal.ZERO.setScale(2);

    @Getter
    @Setter
    @EqualsAndHashCode
    public static class Key implements Serializable {
        private String acctGroupId;
        private String tranTypeCd;
        private Integer tranCatCd;

        public Key() {}

        public Key(String acctGroupId, String tranTypeCd, Integer tranCatCd) {
            this.acctGroupId = acctGroupId;
            this.tranTypeCd = tranTypeCd;
            this.tranCatCd = tranCatCd;
        }
    }
}
