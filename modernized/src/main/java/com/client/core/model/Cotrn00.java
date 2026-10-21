package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: COTRN00
 * @citation app/bms/COTRN00.bms
 * 92 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "cotrn00")
public class Cotrn00 implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * DB2 DCLGEN column COTRN00 DFHMSD
     * PIC clause: X(50)
     */
    @Column(name = "cotrn00")
    @JsonProperty("COTRN00")
    private String cotrn00;

    /**
     * DB2 DCLGEN column COTRN0A DFHMDI
     * PIC clause: X(50)
     */
    @Column(name = "cotrn0a")
    @JsonProperty("COTRN0A")
    private String cotrn0a;

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
     * DB2 DCLGEN column PAGENUM DFHMDF
     * PIC clause: X(50)
     */
    @Id
    @Column(name = "pagenum", nullable = false)
    @JsonProperty("PAGENUM")
    private String pagenum;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf7")
    @JsonProperty("DFHMDF7")
    private String dfhmdf7;

    /**
     * DB2 DCLGEN column TRNIDIN DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trnidin")
    @JsonProperty("TRNIDIN")
    private String trnidin;

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
     * DB2 DCLGEN column DFHMDF ATTRB
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
     * DB2 DCLGEN column DFHMDF ATTRB
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
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf18")
    @JsonProperty("DFHMDF18")
    private String dfhmdf18;

    /**
     * DB2 DCLGEN column SEL0001 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "sel0001")
    @JsonProperty("SEL0001")
    private String sel0001;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf19")
    @JsonProperty("DFHMDF19")
    private String dfhmdf19;

    /**
     * DB2 DCLGEN column TRNID01 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trnid01")
    @JsonProperty("TRNID01")
    private String trnid01;

    /**
     * DB2 DCLGEN column TDATE01 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tdate01")
    @JsonProperty("TDATE01")
    private String tdate01;

    /**
     * DB2 DCLGEN column TDESC01 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tdesc01")
    @JsonProperty("TDESC01")
    private String tdesc01;

    /**
     * DB2 DCLGEN column TAMT001 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tamt001")
    @JsonProperty("TAMT001")
    private String tamt001;

    /**
     * DB2 DCLGEN column SEL0002 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "sel0002")
    @JsonProperty("SEL0002")
    private String sel0002;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf20")
    @JsonProperty("DFHMDF20")
    private String dfhmdf20;

    /**
     * DB2 DCLGEN column TRNID02 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trnid02")
    @JsonProperty("TRNID02")
    private String trnid02;

    /**
     * DB2 DCLGEN column TDATE02 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tdate02")
    @JsonProperty("TDATE02")
    private String tdate02;

    /**
     * DB2 DCLGEN column TDESC02 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tdesc02")
    @JsonProperty("TDESC02")
    private String tdesc02;

    /**
     * DB2 DCLGEN column TAMT002 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tamt002")
    @JsonProperty("TAMT002")
    private String tamt002;

    /**
     * DB2 DCLGEN column SEL0003 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "sel0003")
    @JsonProperty("SEL0003")
    private String sel0003;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf21")
    @JsonProperty("DFHMDF21")
    private String dfhmdf21;

    /**
     * DB2 DCLGEN column TRNID03 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trnid03")
    @JsonProperty("TRNID03")
    private String trnid03;

    /**
     * DB2 DCLGEN column TDATE03 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tdate03")
    @JsonProperty("TDATE03")
    private String tdate03;

    /**
     * DB2 DCLGEN column TDESC03 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tdesc03")
    @JsonProperty("TDESC03")
    private String tdesc03;

    /**
     * DB2 DCLGEN column TAMT003 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tamt003")
    @JsonProperty("TAMT003")
    private String tamt003;

    /**
     * DB2 DCLGEN column SEL0004 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "sel0004")
    @JsonProperty("SEL0004")
    private String sel0004;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf22")
    @JsonProperty("DFHMDF22")
    private String dfhmdf22;

    /**
     * DB2 DCLGEN column TRNID04 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trnid04")
    @JsonProperty("TRNID04")
    private String trnid04;

    /**
     * DB2 DCLGEN column TDATE04 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tdate04")
    @JsonProperty("TDATE04")
    private String tdate04;

    /**
     * DB2 DCLGEN column TDESC04 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tdesc04")
    @JsonProperty("TDESC04")
    private String tdesc04;

    /**
     * DB2 DCLGEN column TAMT004 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tamt004")
    @JsonProperty("TAMT004")
    private String tamt004;

    /**
     * DB2 DCLGEN column SEL0005 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "sel0005")
    @JsonProperty("SEL0005")
    private String sel0005;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf23")
    @JsonProperty("DFHMDF23")
    private String dfhmdf23;

    /**
     * DB2 DCLGEN column TRNID05 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trnid05")
    @JsonProperty("TRNID05")
    private String trnid05;

    /**
     * DB2 DCLGEN column TDATE05 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tdate05")
    @JsonProperty("TDATE05")
    private String tdate05;

    /**
     * DB2 DCLGEN column TDESC05 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tdesc05")
    @JsonProperty("TDESC05")
    private String tdesc05;

    /**
     * DB2 DCLGEN column TAMT005 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tamt005")
    @JsonProperty("TAMT005")
    private String tamt005;

    /**
     * DB2 DCLGEN column SEL0006 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "sel0006")
    @JsonProperty("SEL0006")
    private String sel0006;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf24")
    @JsonProperty("DFHMDF24")
    private String dfhmdf24;

    /**
     * DB2 DCLGEN column TRNID06 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trnid06")
    @JsonProperty("TRNID06")
    private String trnid06;

    /**
     * DB2 DCLGEN column TDATE06 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tdate06")
    @JsonProperty("TDATE06")
    private String tdate06;

    /**
     * DB2 DCLGEN column TDESC06 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tdesc06")
    @JsonProperty("TDESC06")
    private String tdesc06;

    /**
     * DB2 DCLGEN column TAMT006 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tamt006")
    @JsonProperty("TAMT006")
    private String tamt006;

    /**
     * DB2 DCLGEN column SEL0007 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "sel0007")
    @JsonProperty("SEL0007")
    private String sel0007;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf25")
    @JsonProperty("DFHMDF25")
    private String dfhmdf25;

    /**
     * DB2 DCLGEN column TRNID07 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trnid07")
    @JsonProperty("TRNID07")
    private String trnid07;

    /**
     * DB2 DCLGEN column TDATE07 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tdate07")
    @JsonProperty("TDATE07")
    private String tdate07;

    /**
     * DB2 DCLGEN column TDESC07 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tdesc07")
    @JsonProperty("TDESC07")
    private String tdesc07;

    /**
     * DB2 DCLGEN column TAMT007 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tamt007")
    @JsonProperty("TAMT007")
    private String tamt007;

    /**
     * DB2 DCLGEN column SEL0008 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "sel0008")
    @JsonProperty("SEL0008")
    private String sel0008;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf26")
    @JsonProperty("DFHMDF26")
    private String dfhmdf26;

    /**
     * DB2 DCLGEN column TRNID08 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trnid08")
    @JsonProperty("TRNID08")
    private String trnid08;

    /**
     * DB2 DCLGEN column TDATE08 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tdate08")
    @JsonProperty("TDATE08")
    private String tdate08;

    /**
     * DB2 DCLGEN column TDESC08 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tdesc08")
    @JsonProperty("TDESC08")
    private String tdesc08;

    /**
     * DB2 DCLGEN column TAMT008 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tamt008")
    @JsonProperty("TAMT008")
    private String tamt008;

    /**
     * DB2 DCLGEN column SEL0009 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "sel0009")
    @JsonProperty("SEL0009")
    private String sel0009;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf27")
    @JsonProperty("DFHMDF27")
    private String dfhmdf27;

    /**
     * DB2 DCLGEN column TRNID09 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trnid09")
    @JsonProperty("TRNID09")
    private String trnid09;

    /**
     * DB2 DCLGEN column TDATE09 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tdate09")
    @JsonProperty("TDATE09")
    private String tdate09;

    /**
     * DB2 DCLGEN column TDESC09 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tdesc09")
    @JsonProperty("TDESC09")
    private String tdesc09;

    /**
     * DB2 DCLGEN column TAMT009 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tamt009")
    @JsonProperty("TAMT009")
    private String tamt009;

    /**
     * DB2 DCLGEN column SEL0010 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "sel0010")
    @JsonProperty("SEL0010")
    private String sel0010;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf28")
    @JsonProperty("DFHMDF28")
    private String dfhmdf28;

    /**
     * DB2 DCLGEN column TRNID10 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trnid10")
    @JsonProperty("TRNID10")
    private String trnid10;

    /**
     * DB2 DCLGEN column TDATE10 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tdate10")
    @JsonProperty("TDATE10")
    private String tdate10;

    /**
     * DB2 DCLGEN column TDESC10 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tdesc10")
    @JsonProperty("TDESC10")
    private String tdesc10;

    /**
     * DB2 DCLGEN column TAMT010 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "tamt010")
    @JsonProperty("TAMT010")
    private String tamt010;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf29")
    @JsonProperty("DFHMDF29")
    private String dfhmdf29;

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
    @Column(name = "dfhmdf30")
    @JsonProperty("DFHMDF30")
    private String dfhmdf30;

    /**
     * DB2 DCLGEN column DFHMSD TYPE
     * PIC clause: X(50)
     */
    @Column(name = "dfhmsd")
    @JsonProperty("DFHMSD")
    private String dfhmsd;

}
