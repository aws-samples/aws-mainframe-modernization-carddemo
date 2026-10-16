package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: CODATECN
 * @citation app/cpy/CODATECN.cpy
 * 25 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "codatecn")
public class Codatecn implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: CODATECN-TYPE X (line 19)
     * PIC clause: X
     */
    @Id
    @Column(name = "codatecn_type", nullable = false)
    @JsonProperty("CODATECN_TYPE")
    private String codatecnType;

    /**
     * Original: CODATECN-INP-DATE X(20) (line 22)
     * PIC clause: X(20)
     */
    @Column(name = "codatecn_inp_date")
    @JsonProperty("CODATECN_INP_DATE")
    private String codatecnInpDate;

    /**
     * Original: CODATECN-1YYYY XXXX (line 24)
     * PIC clause: XXXX
     */
    @Column(name = "codatecn_1yyyy")
    @JsonProperty("CODATECN_1YYYY")
    private String codatecn1yyyy;

    /**
     * Original: CODATECN-1MM XX (line 25)
     * PIC clause: XX
     */
    @Column(name = "codatecn_1mm")
    @JsonProperty("CODATECN_1MM")
    private String codatecn1mm;

    /**
     * Original: CODATECN-1DD XX (line 26)
     * PIC clause: XX
     */
    @Column(name = "codatecn_1dd")
    @JsonProperty("CODATECN_1DD")
    private String codatecn1dd;

    /**
     * Original: CODATECN-1FIL X(12) (line 27)
     * PIC clause: X(12)
     */
    @Column(name = "codatecn_1fil")
    @JsonProperty("CODATECN_1FIL")
    private String codatecn1fil;

    /**
     * Original: CODATECN-1O-YYYY XXXX (line 29)
     * PIC clause: XXXX
     */
    @Column(name = "codatecn_1o_yyyy")
    @JsonProperty("CODATECN_1O_YYYY")
    private String codatecn1oYyyy;

    /**
     * Original: CODATECN-1I-S1 X (line 30)
     * PIC clause: X
     */
    @Column(name = "codatecn_1i_s1")
    @JsonProperty("CODATECN_1I_S1")
    private String codatecn1iS1;

    /**
     * Original: CODATECN-1MM XX (line 31)
     * PIC clause: XX
     */
    @Column(name = "codatecn_1mm2")
    @JsonProperty("CODATECN_1MM2")
    private String codatecn1mm2;

    /**
     * Original: CODATECN-1I-S2 X (line 32)
     * PIC clause: X
     */
    @Column(name = "codatecn_1i_s2")
    @JsonProperty("CODATECN_1I_S2")
    private String codatecn1iS2;

    /**
     * Original: CODATECN-2YY XX (line 33)
     * PIC clause: XX
     */
    @Column(name = "codatecn_2yy")
    @JsonProperty("CODATECN_2YY")
    private String codatecn2yy;

    /**
     * Original: CODATECN-2FIL X(10) (line 34)
     * PIC clause: X(10)
     */
    @Column(name = "codatecn_2fil")
    @JsonProperty("CODATECN_2FIL")
    private String codatecn2fil;

    /**
     * Original: CODATECN-OUTTYPE X (line 36)
     * PIC clause: X
     */
    @Column(name = "codatecn_outtype")
    @JsonProperty("CODATECN_OUTTYPE")
    private String codatecnOuttype;

    /**
     * Original: CODATECN-0UT-DATE X(20) (line 39)
     * PIC clause: X(20)
     */
    @Column(name = "codatecn_0ut_date")
    @JsonProperty("CODATECN_0UT_DATE")
    private String codatecn0utDate;

    /**
     * Original: CODATECN-1O-YYYY XXXX (line 41)
     * PIC clause: XXXX
     */
    @Column(name = "codatecn_1o_yyyy2")
    @JsonProperty("CODATECN_1O_YYYY2")
    private String codatecn1oYyyy2;

    /**
     * Original: CODATECN-1O-S1 X (line 42)
     * PIC clause: X
     */
    @Column(name = "codatecn_1o_s1")
    @JsonProperty("CODATECN_1O_S1")
    private String codatecn1oS1;

    /**
     * Original: CODATECN-1O-MM XX (line 43)
     * PIC clause: XX
     */
    @Column(name = "codatecn_1o_mm")
    @JsonProperty("CODATECN_1O_MM")
    private String codatecn1oMm;

    /**
     * Original: CODATECN-1O-S2 X (line 44)
     * PIC clause: X
     */
    @Column(name = "codatecn_1o_s2")
    @JsonProperty("CODATECN_1O_S2")
    private String codatecn1oS2;

    /**
     * Original: CODATECN-1O-DD XX (line 45)
     * PIC clause: XX
     */
    @Column(name = "codatecn_1o_dd")
    @JsonProperty("CODATECN_1O_DD")
    private String codatecn1oDd;

    /**
     * Original: CODATECN-1OFIl X(10) (line 46)
     * PIC clause: X(10)
     */
    @Column(name = "codatecn_1ofil")
    @JsonProperty("CODATECN_1OFIL")
    private String codatecn1ofil;

    /**
     * Original: CODATECN-2O-YYYY XXXX (line 48)
     * PIC clause: XXXX
     */
    @Column(name = "codatecn_2o_yyyy")
    @JsonProperty("CODATECN_2O_YYYY")
    private String codatecn2oYyyy;

    /**
     * Original: CODATECN-2O-MM XX (line 49)
     * PIC clause: XX
     */
    @Column(name = "codatecn_2o_mm")
    @JsonProperty("CODATECN_2O_MM")
    private String codatecn2oMm;

    /**
     * Original: CODATECN-2O-DD XX (line 50)
     * PIC clause: XX
     */
    @Column(name = "codatecn_2o_dd")
    @JsonProperty("CODATECN_2O_DD")
    private String codatecn2oDd;

    /**
     * Original: CODATECN-2OFIl X(12) (line 51)
     * PIC clause: X(12)
     */
    @Column(name = "codatecn_2ofil")
    @JsonProperty("CODATECN_2OFIL")
    private String codatecn2ofil;

    /**
     * Original: CODATECN-ERROR-MSG X(38) (line 52)
     * PIC clause: X(38)
     */
    @Column(name = "codatecn_error_msg")
    @JsonProperty("CODATECN_ERROR_MSG")
    private String codatecnErrorMsg;

}
