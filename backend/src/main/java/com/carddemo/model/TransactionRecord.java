package com.carddemo.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.Setter;

/** CVTRA05Y.cpy — TRAN-RECORD (RECLN 350). */
@Entity
@Table(name = "transaction")
@Getter
@Setter
public class TransactionRecord extends TranFields {

    /** TRAN-ID PIC X(16). */
    @Id
    @Column(name = "tran_id", length = 16)
    private String tranId;
}
