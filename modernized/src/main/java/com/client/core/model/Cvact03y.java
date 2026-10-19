package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: CVACT03Y
 * @citation app/cpy/CVACT03Y.cpy
 * 3 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "cvact03y")
public class Cvact03y implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: XREF-CARD-NUM X(16) (line 5)
     * PIC clause: X(16)
     */
    @Id
    @Column(name = "xref_card_num", nullable = false)
    @JsonProperty("XREF_CARD_NUM")
    private String xrefCardNum;

    /**
     * Original: XREF-CUST-ID 9(09) (line 6)
     * PIC clause: 9(09)
     */
    @Column(name = "xref_cust_id")
    @JsonProperty("XREF_CUST_ID")
    private Integer xrefCustId;

    /**
     * Original: XREF-ACCT-ID 9(11) (line 7)
     * PIC clause: 9(11)
     */
    @Column(name = "xref_acct_id")
    @JsonProperty("XREF_ACCT_ID")
    private Long xrefAcctId;

}
