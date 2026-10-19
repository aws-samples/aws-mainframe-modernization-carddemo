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
 * Modernized JPA Entity generated from Copybook: CVTRA04Y
 * @citation app/cpy/CVTRA04Y.cpy
 * 3 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "cvtra04y")
public class Cvtra04y implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: TRAN-TYPE-CD X(02) (line 6)
     * PIC clause: X(02)
     */
    @Id
    @Column(name = "tran_type_cd", nullable = false)
    @JsonProperty("TRAN_TYPE_CD")
    private String tranTypeCd;

    /**
     * Original: TRAN-CAT-CD 9(04) (line 7)
     * PIC clause: 9(04)
     */
    @Column(name = "tran_cat_cd")
    @JsonProperty("TRAN_CAT_CD")
    private Integer tranCatCd;

    /**
     * Original: TRAN-CAT-TYPE-DESC X(50) (line 8)
     * PIC clause: X(50)
     */
    @Column(name = "tran_cat_type_desc")
    @JsonProperty("TRAN_CAT_TYPE_DESC")
    private String tranCatTypeDesc;

    /**
     * Unique Transaction Identifier
     * Note: Transaction posting attribute.
     */
    @Transient
    @JsonProperty("TRAN_ID")
    private String tranId;

    /**
     * Card PAN Used in Transaction
     * Note: Transaction posting attribute.
     */
    @Transient
    @JsonProperty("CARD_NUM")
    private String cardNum;

    /**
     * Authorized Transaction Amount
     * Note: Transaction posting attribute.
     */
    @Transient
    @JsonProperty("TRAN_AMT")
    private BigDecimal tranAmt;

    /**
     * Original Transaction Timestamp
     * Note: Transaction posting attribute.
     */
    @Transient
    @JsonProperty("ORIG_TS")
    private String origTs;

}
