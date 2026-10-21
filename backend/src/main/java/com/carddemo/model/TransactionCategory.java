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

/** CVTRA04Y.cpy — TRAN-CAT-RECORD (RECLN 60). */
@Entity
@Table(name = "transaction_category")
@IdClass(TransactionCategory.Key.class)
@Getter
@Setter
public class TransactionCategory {

    /** TRAN-TYPE-CD PIC X(02). */
    @Id
    @Column(name = "type_cd", length = 2)
    private String typeCd;

    /** TRAN-CAT-CD PIC 9(04). */
    @Id
    @Column(name = "cat_cd")
    private Integer catCd;

    /** TRAN-CAT-TYPE-DESC PIC X(50). */
    @Column(name = "cat_type_desc", length = 50)
    private String catTypeDesc;

    @Getter
    @Setter
    @EqualsAndHashCode
    public static class Key implements Serializable {
        private String typeCd;
        private Integer catCd;

        public Key() {}

        public Key(String typeCd, Integer catCd) {
            this.typeCd = typeCd;
            this.catCd = catCd;
        }
    }
}
