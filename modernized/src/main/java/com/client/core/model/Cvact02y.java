package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: CVACT02Y
 * @citation app/cpy/CVACT02Y.cpy
 * 6 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "cvact02y")
public class Cvact02y implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: CARD-NUM X(16) (line 5)
     * PIC clause: X(16)
     */
    @Id
    @Column(name = "card_num", nullable = false)
    @JsonProperty("CARD_NUM")
    private String cardNum;

    /**
     * Original: CARD-ACCT-ID 9(11) (line 6)
     * PIC clause: 9(11)
     */
    @Column(name = "card_acct_id")
    @JsonProperty("CARD_ACCT_ID")
    private Long cardAcctId;

    /**
     * Original: CARD-CVV-CD 9(03) (line 7)
     * PIC clause: 9(03)
     * Note: PCI-DSS 3.2 Prohibited Storage - Excluded from persistent database columns.
     */
    @Transient
    @JsonProperty("CARD_CVV_CD")
    private Integer cardCvvCd;

    /**
     * Original: CARD-EMBOSSED-NAME X(50) (line 8)
     * PIC clause: X(50)
     */
    @Column(name = "card_embossed_name")
    @JsonProperty("CARD_EMBOSSED_NAME")
    private String cardEmbossedName;

    /**
     * Original: CARD-EXPIRAION-DATE X(10) (line 9)
     * PIC clause: X(10)
     */
    @Column(name = "card_expiraion_date")
    @JsonProperty("CARD_EXPIRAION_DATE")
    private String cardExpiraionDate;

    /**
     * Original: CARD-ACTIVE-STATUS X(01) (line 10)
     * PIC clause: X(01)
     */
    @Column(name = "card_active_status")
    @JsonProperty("CARD_ACTIVE_STATUS")
    private String cardActiveStatus;

}
