package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: CSDB2RPY
 * @citation app/app-transaction-type-db2/cpy/CSDB2RPY.cpy
 * 53 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "csdb2rpy")
public class Csdb2rpy implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * DB2 DCLGEN column 177100 9998
     * PIC clause: X(50)
     */
    @Id
    @Column(name = "177100", nullable = false)
    @JsonProperty("177100")
    private String m177100;

    /**
     * DB2 DCLGEN column 177200 17720000
     * PIC clause: X(50)
     */
    @Column(name = "177200")
    @JsonProperty("177200")
    private String m177200;

    /**
     * DB2 DCLGEN column 177300 EXEC
     * PIC clause: X(50)
     */
    @Column(name = "177300")
    @JsonProperty("177300")
    private String m177300;

    /**
     * DB2 DCLGEN column 177500 SELECT
     * PIC clause: X(50)
     */
    @Column(name = "177500")
    @JsonProperty("177500")
    private String m177500;

    /**
     * DB2 DCLGEN column 177600 INTO
     * PIC clause: S9(9) COMP
     */
    @Column(name = "177600")
    @JsonProperty("177600")
    private Integer m177600;

    /**
     * DB2 DCLGEN column 177700 FROM
     * PIC clause: X(50)
     */
    @Column(name = "177700")
    @JsonProperty("177700")
    private String m177700;

    /**
     * DB2 DCLGEN column 177900 FETCH
     * PIC clause: X(50)
     */
    @Column(name = "177900")
    @JsonProperty("177900")
    private String m177900;

    /**
     * DB2 DCLGEN column 178000 END
     * PIC clause: X(50)
     */
    @Column(name = "178000")
    @JsonProperty("178000")
    private String m178000;

    /**
     * DB2 DCLGEN column 178100 17810000
     * PIC clause: X(50)
     */
    @Column(name = "178100")
    @JsonProperty("178100")
    private String m178100;

    /**
     * DB2 DCLGEN column 178200 17820000
     * PIC clause: X(50)
     */
    @Column(name = "178200")
    @JsonProperty("178200")
    private String m178200;

    /**
     * DB2 DCLGEN column 178300 MOVE
     * PIC clause: X(50)
     */
    @Column(name = "178300")
    @JsonProperty("178300")
    private String m178300;

    /**
     * DB2 DCLGEN column 178400 17840000
     * PIC clause: X(50)
     */
    @Column(name = "178400")
    @JsonProperty("178400")
    private String m178400;

    /**
     * DB2 DCLGEN column 178500 EVALUATE
     * PIC clause: X(50)
     */
    @Column(name = "178500")
    @JsonProperty("178500")
    private String m178500;

    /**
     * DB2 DCLGEN column 178600 WHEN
     * PIC clause: X(50)
     */
    @Column(name = "178600")
    @JsonProperty("178600")
    private String m178600;

    /**
     * DB2 DCLGEN column 178700 CONTINUE
     * PIC clause: X(50)
     */
    @Column(name = "178700")
    @JsonProperty("178700")
    private String m178700;

    /**
     * DB2 DCLGEN column 178800 WHEN
     * PIC clause: X(50)
     */
    @Column(name = "178800")
    @JsonProperty("178800")
    private String m178800;

    /**
     * DB2 DCLGEN column 179000 SET
     * PIC clause: X(50)
     */
    @Column(name = "179000")
    @JsonProperty("179000")
    private String m179000;

    /**
     * DB2 DCLGEN column 179100 17910000
     * PIC clause: X(50)
     */
    @Column(name = "179100")
    @JsonProperty("179100")
    private String m179100;

    /**
     * DB2 DCLGEN column 179200 MOVE
     * PIC clause: X(50)
     */
    @Column(name = "179200")
    @JsonProperty("179200")
    private String m179200;

    /**
     * DB2 DCLGEN column 179300 TO
     * PIC clause: X(50)
     */
    @Column(name = "179300")
    @JsonProperty("179300")
    private String m179300;

    /**
     * DB2 DCLGEN column 179400 PERFORM
     * PIC clause: X(50)
     */
    @Column(name = "179400")
    @JsonProperty("179400")
    private String m179400;

    /**
     * DB2 DCLGEN column 179500 THRU
     * PIC clause: X(50)
     */
    @Column(name = "179500")
    @JsonProperty("179500")
    private String m179500;

    /**
     * DB2 DCLGEN column 179600 END
     * PIC clause: X(50)
     */
    @Column(name = "179600")
    @JsonProperty("179600")
    private String m179600;

    /**
     * DB2 DCLGEN column 179800 9998
     * PIC clause: X(50)
     */
    @Column(name = "179800")
    @JsonProperty("179800")
    private String m179800;

    /**
     * DB2 DCLGEN column 179900 EXIT
     * PIC clause: X(50)
     */
    @Column(name = "179900")
    @JsonProperty("179900")
    private String m179900;

    /**
     * DB2 DCLGEN column 205700 9999
     * PIC clause: X(50)
     */
    @Column(name = "205700")
    @JsonProperty("205700")
    private String m205700;

    /**
     * DB2 DCLGEN column CALL LIT
     * PIC clause: X(50)
     */
    @Column(name = "call")
    @JsonProperty("CALL")
    private String call;

    /**
     * DB2 DCLGEN column 206000 DFHCOMMAREA
     * PIC clause: X(50)
     */
    @Column(name = "206000")
    @JsonProperty("206000")
    private String m206000;

    /**
     * DB2 DCLGEN column 206100 SQLCA
     * PIC clause: X(50)
     */
    @Column(name = "206100")
    @JsonProperty("206100")
    private String m206100;

    /**
     * DB2 DCLGEN column 206200 WS
     * PIC clause: X(50)
     */
    @Column(name = "206200")
    @JsonProperty("206200")
    private String m206200;

    /**
     * DB2 DCLGEN column 206300 WS
     * PIC clause: X(50)
     */
    @Column(name = "206300")
    @JsonProperty("206300")
    private String m206300;

    /**
     * DB2 DCLGEN column 06400 END
     * PIC clause: X(50)
     */
    @Column(name = "06400")
    @JsonProperty("06400")
    private String m06400;

    /**
     * DB2 DCLGEN column 206401 20640100
     * PIC clause: X(50)
     */
    @Column(name = "206401")
    @JsonProperty("206401")
    private String m206401;

    /**
     * DB2 DCLGEN column 206402 COMPUTE
     * PIC clause: X(50)
     */
    @Column(name = "206402")
    @JsonProperty("206402")
    private String m206402;

    /**
     * DB2 DCLGEN column 206403 20640301
     * PIC clause: X(50)
     */
    @Column(name = "206403")
    @JsonProperty("206403")
    private String m206403;

    /**
     * DB2 DCLGEN column 206410 IF
     * PIC clause: X(50)
     */
    @Column(name = "206410")
    @JsonProperty("206410")
    private String m206410;

    /**
     * DB2 DCLGEN column 206420 CONTINUE
     * PIC clause: X(50)
     */
    @Column(name = "206420")
    @JsonProperty("206420")
    private String m206420;

    /**
     * DB2 DCLGEN column 206430 ELSE
     * PIC clause: X(50)
     */
    @Column(name = "206430")
    @JsonProperty("206430")
    private String m206430;

    /**
     * DB2 DCLGEN column 206431 MOVE
     * PIC clause: X(50)
     */
    @Column(name = "206431")
    @JsonProperty("206431")
    private String m206431;

    /**
     * DB2 DCLGEN column 206432 MOVE
     * PIC clause: X(50)
     */
    @Column(name = "206432")
    @JsonProperty("206432")
    private String m206432;

    /**
     * DB2 DCLGEN column 206450 END
     * PIC clause: X(50)
     */
    @Column(name = "206450")
    @JsonProperty("206450")
    private String m206450;

    /**
     * DB2 DCLGEN column 206500 20650000
     * PIC clause: X(50)
     */
    @Column(name = "206500")
    @JsonProperty("206500")
    private String m206500;

    /**
     * DB2 DCLGEN column 206600 STRING
     * PIC clause: X(50)
     */
    @Column(name = "206600")
    @JsonProperty("206600")
    private String m206600;

    /**
     * DB2 DCLGEN column 206700 FUNCTION
     * PIC clause: X(50)
     */
    @Column(name = "206700")
    @JsonProperty("206700")
    private String m206700;

    /**
     * DB2 DCLGEN column 206900 WS
     * PIC clause: X(50)
     */
    @Column(name = "206900")
    @JsonProperty("206900")
    private String m206900;

    /**
     * DB2 DCLGEN column 207200 WS
     * PIC clause: X(50)
     */
    @Column(name = "207200")
    @JsonProperty("207200")
    private String m207200;

    /**
     * DB2 DCLGEN column 207300 DELIMITED
     * PIC clause: X(50)
     */
    @Column(name = "207300")
    @JsonProperty("207300")
    private String m207300;

    /**
     * DB2 DCLGEN column 207400 INTO
     * PIC clause: S9(9) COMP
     */
    @Column(name = "207400")
    @JsonProperty("207400")
    private Integer m207400;

    /**
     * DB2 DCLGEN column 207500 END
     * PIC clause: X(50)
     */
    @Column(name = "207500")
    @JsonProperty("207500")
    private String m207500;

    /**
     * DB2 DCLGEN column 207600 MOVE
     * PIC clause: X(50)
     */
    @Column(name = "207600")
    @JsonProperty("207600")
    private String m207600;

    /**
     * DB2 DCLGEN column 207800 20780000
     * PIC clause: X(50)
     */
    @Column(name = "207800")
    @JsonProperty("207800")
    private String m207800;

    /**
     * DB2 DCLGEN column 207900 9999
     * PIC clause: X(50)
     */
    @Column(name = "207900")
    @JsonProperty("207900")
    private String m207900;

    /**
     * DB2 DCLGEN column 208000 EXIT
     * PIC clause: X(50)
     */
    @Column(name = "208000")
    @JsonProperty("208000")
    private String m208000;

}
