package com.carddemo.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.Setter;

/** CVTRA06Y.cpy — DALYTRAN-RECORD (RECLN 350). Staging table fed to the
 *  posting batch job (CBTRN02C). */
@Entity
@Table(name = "daily_transaction")
@Getter
@Setter
public class DailyTransaction extends TranFields {

    /** DALYTRAN-ID PIC X(16). */
    @Id
    @Column(name = "tran_id", length = 16)
    private String tranId;
}
