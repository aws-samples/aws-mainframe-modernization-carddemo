package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: COCOM01Y
 * @citation app/cpy/COCOM01Y.cpy
 * 16 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "cocom01y")
public class Cocom01y implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: CDEMO-FROM-TRANID X(04) (line 21)
     * PIC clause: X(04)
     */
    @Id
    @Column(name = "cdemo_from_tranid", nullable = false)
    @JsonProperty("CDEMO_FROM_TRANID")
    private String cdemoFromTranid;

    /**
     * Original: CDEMO-FROM-PROGRAM X(08) (line 22)
     * PIC clause: X(08)
     */
    @Column(name = "cdemo_from_program")
    @JsonProperty("CDEMO_FROM_PROGRAM")
    private String cdemoFromProgram;

    /**
     * Original: CDEMO-TO-TRANID X(04) (line 23)
     * PIC clause: X(04)
     */
    @Column(name = "cdemo_to_tranid")
    @JsonProperty("CDEMO_TO_TRANID")
    private String cdemoToTranid;

    /**
     * Original: CDEMO-TO-PROGRAM X(08) (line 24)
     * PIC clause: X(08)
     */
    @Column(name = "cdemo_to_program")
    @JsonProperty("CDEMO_TO_PROGRAM")
    private String cdemoToProgram;

    /**
     * Original: CDEMO-USER-ID X(08) (line 25)
     * PIC clause: X(08)
     */
    @Column(name = "cdemo_user_id")
    @JsonProperty("CDEMO_USER_ID")
    private String cdemoUserId;

    /**
     * Original: CDEMO-USER-TYPE X(01) (line 26)
     * PIC clause: X(01)
     */
    @Column(name = "cdemo_user_type")
    @JsonProperty("CDEMO_USER_TYPE")
    private String cdemoUserType;

    /**
     * Original: CDEMO-PGM-CONTEXT 9(01) (line 29)
     * PIC clause: 9(01)
     */
    @Column(name = "cdemo_pgm_context")
    @JsonProperty("CDEMO_PGM_CONTEXT")
    private Integer cdemoPgmContext;

    /**
     * Original: CDEMO-CUST-ID 9(09) (line 33)
     * PIC clause: 9(09)
     */
    @Column(name = "cdemo_cust_id")
    @JsonProperty("CDEMO_CUST_ID")
    private Integer cdemoCustId;

    /**
     * Original: CDEMO-CUST-FNAME X(25) (line 34)
     * PIC clause: X(25)
     */
    @Column(name = "cdemo_cust_fname")
    @JsonProperty("CDEMO_CUST_FNAME")
    private String cdemoCustFname;

    /**
     * Original: CDEMO-CUST-MNAME X(25) (line 35)
     * PIC clause: X(25)
     */
    @Column(name = "cdemo_cust_mname")
    @JsonProperty("CDEMO_CUST_MNAME")
    private String cdemoCustMname;

    /**
     * Original: CDEMO-CUST-LNAME X(25) (line 36)
     * PIC clause: X(25)
     */
    @Column(name = "cdemo_cust_lname")
    @JsonProperty("CDEMO_CUST_LNAME")
    private String cdemoCustLname;

    /**
     * Original: CDEMO-ACCT-ID 9(11) (line 38)
     * PIC clause: 9(11)
     */
    @Column(name = "cdemo_acct_id")
    @JsonProperty("CDEMO_ACCT_ID")
    private Long cdemoAcctId;

    /**
     * Original: CDEMO-ACCT-STATUS X(01) (line 39)
     * PIC clause: X(01)
     */
    @Column(name = "cdemo_acct_status")
    @JsonProperty("CDEMO_ACCT_STATUS")
    private String cdemoAcctStatus;

    /**
     * Original: CDEMO-CARD-NUM 9(16) (line 41)
     * PIC clause: 9(16)
     */
    @Column(name = "cdemo_card_num")
    @JsonProperty("CDEMO_CARD_NUM")
    private Long cdemoCardNum;

    /**
     * Original: CDEMO-LAST-MAP X(7) (line 43)
     * PIC clause: X(7)
     */
    @Column(name = "cdemo_last_map")
    @JsonProperty("CDEMO_LAST_MAP")
    private String cdemoLastMap;

    /**
     * Original: CDEMO-LAST-MAPSET X(7) (line 44)
     * PIC clause: X(7)
     */
    @Column(name = "cdemo_last_mapset")
    @JsonProperty("CDEMO_LAST_MAPSET")
    private String cdemoLastMapset;

}
