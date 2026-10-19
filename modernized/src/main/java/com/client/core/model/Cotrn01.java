package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: COTRN01
 * @citation app/bms/COTRN01.bms
 * 59 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "cotrn01")
public class Cotrn01 implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * DB2 DCLGEN column COTRN01 DFHMSD
     * PIC clause: X(50)
     */
    @Column(name = "cotrn01")
    @JsonProperty("COTRN01")
    private String cotrn01;

    /**
     * DB2 DCLGEN column COTRN1A DFHMDI
     * PIC clause: X(50)
     */
    @Column(name = "cotrn1a")
    @JsonProperty("COTRN1A")
    private String cotrn1a;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf")
    @JsonProperty("DFHMDF")
    private String dfhmdf;

    /**
     * DB2 DCLGEN column TRNNAME DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trnname")
    @JsonProperty("TRNNAME")
    private String trnname;

    /**
     * DB2 DCLGEN column TITLE01 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "title01")
    @JsonProperty("TITLE01")
    private String title01;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf2")
    @JsonProperty("DFHMDF2")
    private String dfhmdf2;

    /**
     * DB2 DCLGEN column CURDATE DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "curdate")
    @JsonProperty("CURDATE")
    private String curdate;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf3")
    @JsonProperty("DFHMDF3")
    private String dfhmdf3;

    /**
     * DB2 DCLGEN column PGMNAME DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "pgmname")
    @JsonProperty("PGMNAME")
    private String pgmname;

    /**
     * DB2 DCLGEN column TITLE02 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "title02")
    @JsonProperty("TITLE02")
    private String title02;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf4")
    @JsonProperty("DFHMDF4")
    private String dfhmdf4;

    /**
     * DB2 DCLGEN column CURTIME DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "curtime")
    @JsonProperty("CURTIME")
    private String curtime;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf5")
    @JsonProperty("DFHMDF5")
    private String dfhmdf5;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf6")
    @JsonProperty("DFHMDF6")
    private String dfhmdf6;

    /**
     * DB2 DCLGEN column TRNIDIN DFHMDF
     * PIC clause: X(50)
     */
    @Id
    @Column(name = "trnidin", nullable = false)
    @JsonProperty("TRNIDIN")
    private String trnidin;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf7")
    @JsonProperty("DFHMDF7")
    private String dfhmdf7;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf8")
    @JsonProperty("DFHMDF8")
    private String dfhmdf8;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf9")
    @JsonProperty("DFHMDF9")
    private String dfhmdf9;

    /**
     * DB2 DCLGEN column TRNID DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trnid")
    @JsonProperty("TRNID")
    private String trnid;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf10")
    @JsonProperty("DFHMDF10")
    private String dfhmdf10;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf11")
    @JsonProperty("DFHMDF11")
    private String dfhmdf11;

    /**
     * DB2 DCLGEN column CARDNUM DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "cardnum")
    @JsonProperty("CARDNUM")
    private String cardnum;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf12")
    @JsonProperty("DFHMDF12")
    private String dfhmdf12;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf13")
    @JsonProperty("DFHMDF13")
    private String dfhmdf13;

    /**
     * DB2 DCLGEN column TTYPCD DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "ttypcd")
    @JsonProperty("TTYPCD")
    private String ttypcd;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf14")
    @JsonProperty("DFHMDF14")
    private String dfhmdf14;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf15")
    @JsonProperty("DFHMDF15")
    private String dfhmdf15;

    /**
     * DB2 DCLGEN column TCATCD DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tcatcd")
    @JsonProperty("TCATCD")
    private String tcatcd;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf16")
    @JsonProperty("DFHMDF16")
    private String dfhmdf16;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf17")
    @JsonProperty("DFHMDF17")
    private String dfhmdf17;

    /**
     * DB2 DCLGEN column TRNSRC DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trnsrc")
    @JsonProperty("TRNSRC")
    private String trnsrc;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf18")
    @JsonProperty("DFHMDF18")
    private String dfhmdf18;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf19")
    @JsonProperty("DFHMDF19")
    private String dfhmdf19;

    /**
     * DB2 DCLGEN column TDESC DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tdesc")
    @JsonProperty("TDESC")
    private String tdesc;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf20")
    @JsonProperty("DFHMDF20")
    private String dfhmdf20;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf21")
    @JsonProperty("DFHMDF21")
    private String dfhmdf21;

    /**
     * DB2 DCLGEN column TRNAMT DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trnamt")
    @JsonProperty("TRNAMT")
    private String trnamt;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf22")
    @JsonProperty("DFHMDF22")
    private String dfhmdf22;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf23")
    @JsonProperty("DFHMDF23")
    private String dfhmdf23;

    /**
     * DB2 DCLGEN column TORIGDT DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "torigdt")
    @JsonProperty("TORIGDT")
    private String torigdt;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf24")
    @JsonProperty("DFHMDF24")
    private String dfhmdf24;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf25")
    @JsonProperty("DFHMDF25")
    private String dfhmdf25;

    /**
     * DB2 DCLGEN column TPROCDT DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tprocdt")
    @JsonProperty("TPROCDT")
    private String tprocdt;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf26")
    @JsonProperty("DFHMDF26")
    private String dfhmdf26;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf27")
    @JsonProperty("DFHMDF27")
    private String dfhmdf27;

    /**
     * DB2 DCLGEN column MID DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "mid")
    @JsonProperty("MID")
    private String mid;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf28")
    @JsonProperty("DFHMDF28")
    private String dfhmdf28;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf29")
    @JsonProperty("DFHMDF29")
    private String dfhmdf29;

    /**
     * DB2 DCLGEN column MNAME DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "mname")
    @JsonProperty("MNAME")
    private String mname;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf30")
    @JsonProperty("DFHMDF30")
    private String dfhmdf30;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf31")
    @JsonProperty("DFHMDF31")
    private String dfhmdf31;

    /**
     * DB2 DCLGEN column MCITY DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "mcity")
    @JsonProperty("MCITY")
    private String mcity;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf32")
    @JsonProperty("DFHMDF32")
    private String dfhmdf32;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf33")
    @JsonProperty("DFHMDF33")
    private String dfhmdf33;

    /**
     * DB2 DCLGEN column MZIP DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "mzip")
    @JsonProperty("MZIP")
    private String mzip;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf34")
    @JsonProperty("DFHMDF34")
    private String dfhmdf34;

    /**
     * DB2 DCLGEN column ERRMSG DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "errmsg")
    @JsonProperty("ERRMSG")
    private String errmsg;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf35")
    @JsonProperty("DFHMDF35")
    private String dfhmdf35;

    /**
     * DB2 DCLGEN column DFHMSD TYPE
     * PIC clause: X(50)
     */
    @Column(name = "dfhmsd")
    @JsonProperty("DFHMSD")
    private String dfhmsd;

}
