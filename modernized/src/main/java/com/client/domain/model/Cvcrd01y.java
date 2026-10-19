package com.client.domain.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: CVCRD01Y
 * @citation cpy/CVCRD01Y.cpy
 * 12 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "cvcrd01y")
public class Cvcrd01y implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: CCARD-AID X(5) (line 3)
     * PIC clause: X(5)
     */
    @Id
    @Column(name = "ccard_aid", nullable = false)
    @JsonProperty("CCARD_AID")
    private String ccardAid;

    /**
     * Original: CCARD-NEXT-PROG X(8) (line 21)
     * PIC clause: X(8)
     */
    @Column(name = "ccard_next_prog")
    @JsonProperty("CCARD_NEXT_PROG")
    private String ccardNextProg;

    /**
     * Original: CCARD-NEXT-MAPSET X(7) (line 23)
     * PIC clause: X(7)
     */
    @Column(name = "ccard_next_mapset")
    @JsonProperty("CCARD_NEXT_MAPSET")
    private String ccardNextMapset;

    /**
     * Original: CCARD-NEXT-MAP X(7) (line 24)
     * PIC clause: X(7)
     */
    @Column(name = "ccard_next_map")
    @JsonProperty("CCARD_NEXT_MAP")
    private String ccardNextMap;

    /**
     * Original: CCARD-ERROR-MSG X(75) (line 28)
     * PIC clause: X(75)
     */
    @Column(name = "ccard_error_msg")
    @JsonProperty("CCARD_ERROR_MSG")
    private String ccardErrorMsg;

    /**
     * Original: CCARD-RETURN-MSG X(75) (line 29)
     * PIC clause: X(75)
     */
    @Column(name = "ccard_return_msg")
    @JsonProperty("CCARD_RETURN_MSG")
    private String ccardReturnMsg;

    /**
     * Original: CC-ACCT-ID X(11) (line 34)
     * PIC clause: X(11)
     */
    @Column(name = "cc_acct_id")
    @JsonProperty("CC_ACCT_ID")
    private String ccAcctId;

    /**
     * Original: CC-ACCT-ID-N 9(11) (line 36)
     * PIC clause: 9(11)
     */
    @Column(name = "cc_acct_id_n")
    @JsonProperty("CC_ACCT_ID_N")
    private Long ccAcctIdN;

    /**
     * Original: CC-CARD-NUM X(16) (line 37)
     * PIC clause: X(16)
     */
    @Column(name = "cc_card_num")
    @JsonProperty("CC_CARD_NUM")
    private String ccCardNum;

    /**
     * Original: CC-CARD-NUM-N 9(16) (line 39)
     * PIC clause: 9(16)
     */
    @Column(name = "cc_card_num_n")
    @JsonProperty("CC_CARD_NUM_N")
    private Long ccCardNumN;

    /**
     * Original: CC-CUST-ID X(09) (line 40)
     * PIC clause: X(09)
     */
    @Column(name = "cc_cust_id")
    @JsonProperty("CC_CUST_ID")
    private String ccCustId;

    /**
     * Original: CC-CUST-ID-N 9(9) (line 42)
     * PIC clause: 9(9)
     */
    @Column(name = "cc_cust_id_n")
    @JsonProperty("CC_CUST_ID_N")
    private Integer ccCustIdN;

}
