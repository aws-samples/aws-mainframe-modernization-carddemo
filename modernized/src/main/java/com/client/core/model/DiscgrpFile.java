package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;
import java.math.BigDecimal;

/**
 * Modernized JPA Entity generated from Copybook: DISCGRP_FILE
 * @citation DISCGRP_FILE (source not found)
 * 10 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "discgrp_file")
public class DiscgrpFile implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Statement or Balance Record ID
     * PIC clause: X(16)
     */
    @Id
    @Column(name = "record_id", nullable = false)
    @JsonProperty("RECORD_ID")
    private String recordId;

    /**
     * Associated Account ID
     * PIC clause: 9(11)
     */
    @Column(name = "acct_id")
    @JsonProperty("ACCT_ID")
    private Long acctId;

    /**
     * Discount / Interest Group ID
     * PIC clause: X(10)
     */
    @Column(name = "disc_group_id")
    @JsonProperty("DISC_GROUP_ID")
    private String discGroupId;

    /**
     * Transaction Type Code
     * PIC clause: X(02)
     */
    @Column(name = "tran_type_cd")
    @JsonProperty("TRAN_TYPE_CD")
    private String tranTypeCd;

    /**
     * Transaction Category Code
     * PIC clause: 9(04)
     */
    @Column(name = "tran_cat_cd")
    @JsonProperty("TRAN_CAT_CD")
    private Integer tranCatCd;

    /**
     * Category Aggregate Balance
     * PIC clause: S9(10)V99
     */
    @Column(name = "cat_bal")
    @JsonProperty("CAT_BAL")
    private BigDecimal catBal;

    /**
     * Annual Interest / Discount Rate
     * PIC clause: S9(03)V99
     */
    @Column(name = "disc_rate")
    @JsonProperty("DISC_RATE")
    private BigDecimal discRate;

    /**
     * Minimum Balance Threshold
     * PIC clause: S9(08)V99
     */
    @Column(name = "min_bal")
    @JsonProperty("MIN_BAL")
    private BigDecimal minBal;

    /**
     * Statement Billing Cycle Date
     * PIC clause: X(10)
     */
    @Column(name = "statement_date")
    @JsonProperty("STATEMENT_DATE")
    private String statementDate;

    /**
     * Net Activity Amount
     * PIC clause: S9(10)V99
     */
    @Column(name = "net_activity")
    @JsonProperty("NET_ACTIVITY")
    private BigDecimal netActivity;

}
