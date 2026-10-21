package com.carddemo.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.Setter;

/** CVTRA03Y.cpy — TRAN-TYPE-RECORD (RECLN 60). */
@Entity
@Table(name = "transaction_type")
@Getter
@Setter
public class TransactionType {

    /** TRAN-TYPE PIC X(02). */
    @Id
    @Column(name = "type_cd", length = 2)
    private String typeCd;

    /** TRAN-TYPE-DESC PIC X(50). */
    @Column(name = "type_desc", length = 50)
    private String typeDesc;
}
