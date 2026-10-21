package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: COUSR00
 * @citation app/bms/COUSR00.bms
 * 93 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "cousr00")
public class Cousr00 implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * DB2 DCLGEN column COUSR00 DFHMSD
     * PIC clause: X(50)
     */
    @Column(name = "cousr00")
    @JsonProperty("COUSR00")
    private String cousr00;

    /**
     * DB2 DCLGEN column COUSR0A DFHMDI
     * PIC clause: X(50)
     */
    @Column(name = "cousr0a")
    @JsonProperty("COUSR0A")
    private String cousr0a;

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
     * DB2 DCLGEN column USRIDIN DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "usridin")
    @JsonProperty("USRIDIN")
    private String usridin;

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
     * DB2 DCLGEN column USRID01 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "usrid01")
    @JsonProperty("USRID01")
    private String usrid01;

    /**
     * DB2 DCLGEN column FNAME01 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "fname01")
    @JsonProperty("FNAME01")
    private String fname01;

    /**
     * DB2 DCLGEN column LNAME01 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "lname01")
    @JsonProperty("LNAME01")
    private String lname01;

    /**
     * DB2 DCLGEN column UTYPE01 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "utype01")
    @JsonProperty("UTYPE01")
    private String utype01;

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
     * DB2 DCLGEN column USRID02 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "usrid02")
    @JsonProperty("USRID02")
    private String usrid02;

    /**
     * DB2 DCLGEN column FNAME02 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "fname02")
    @JsonProperty("FNAME02")
    private String fname02;

    /**
     * DB2 DCLGEN column LNAME02 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "lname02")
    @JsonProperty("LNAME02")
    private String lname02;

    /**
     * DB2 DCLGEN column UTYPE02 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "utype02")
    @JsonProperty("UTYPE02")
    private String utype02;

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
     * DB2 DCLGEN column USRID03 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "usrid03")
    @JsonProperty("USRID03")
    private String usrid03;

    /**
     * DB2 DCLGEN column FNAME03 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "fname03")
    @JsonProperty("FNAME03")
    private String fname03;

    /**
     * DB2 DCLGEN column LNAME03 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "lname03")
    @JsonProperty("LNAME03")
    private String lname03;

    /**
     * DB2 DCLGEN column UTYPE03 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "utype03")
    @JsonProperty("UTYPE03")
    private String utype03;

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
     * DB2 DCLGEN column USRID04 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "usrid04")
    @JsonProperty("USRID04")
    private String usrid04;

    /**
     * DB2 DCLGEN column FNAME04 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "fname04")
    @JsonProperty("FNAME04")
    private String fname04;

    /**
     * DB2 DCLGEN column LNAME04 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "lname04")
    @JsonProperty("LNAME04")
    private String lname04;

    /**
     * DB2 DCLGEN column UTYPE04 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "utype04")
    @JsonProperty("UTYPE04")
    private String utype04;

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
     * DB2 DCLGEN column USRID05 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "usrid05")
    @JsonProperty("USRID05")
    private String usrid05;

    /**
     * DB2 DCLGEN column FNAME05 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "fname05")
    @JsonProperty("FNAME05")
    private String fname05;

    /**
     * DB2 DCLGEN column LNAME05 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "lname05")
    @JsonProperty("LNAME05")
    private String lname05;

    /**
     * DB2 DCLGEN column UTYPE05 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "utype05")
    @JsonProperty("UTYPE05")
    private String utype05;

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
     * DB2 DCLGEN column USRID06 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "usrid06")
    @JsonProperty("USRID06")
    private String usrid06;

    /**
     * DB2 DCLGEN column FNAME06 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "fname06")
    @JsonProperty("FNAME06")
    private String fname06;

    /**
     * DB2 DCLGEN column LNAME06 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "lname06")
    @JsonProperty("LNAME06")
    private String lname06;

    /**
     * DB2 DCLGEN column UTYPE06 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "utype06")
    @JsonProperty("UTYPE06")
    private String utype06;

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
     * DB2 DCLGEN column USRID07 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "usrid07")
    @JsonProperty("USRID07")
    private String usrid07;

    /**
     * DB2 DCLGEN column FNAME07 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "fname07")
    @JsonProperty("FNAME07")
    private String fname07;

    /**
     * DB2 DCLGEN column LNAME07 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "lname07")
    @JsonProperty("LNAME07")
    private String lname07;

    /**
     * DB2 DCLGEN column UTYPE07 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "utype07")
    @JsonProperty("UTYPE07")
    private String utype07;

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
     * DB2 DCLGEN column USRID08 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "usrid08")
    @JsonProperty("USRID08")
    private String usrid08;

    /**
     * DB2 DCLGEN column FNAME08 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "fname08")
    @JsonProperty("FNAME08")
    private String fname08;

    /**
     * DB2 DCLGEN column LNAME08 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "lname08")
    @JsonProperty("LNAME08")
    private String lname08;

    /**
     * DB2 DCLGEN column UTYPE08 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "utype08")
    @JsonProperty("UTYPE08")
    private String utype08;

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
     * DB2 DCLGEN column USRID09 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "usrid09")
    @JsonProperty("USRID09")
    private String usrid09;

    /**
     * DB2 DCLGEN column FNAME09 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "fname09")
    @JsonProperty("FNAME09")
    private String fname09;

    /**
     * DB2 DCLGEN column LNAME09 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "lname09")
    @JsonProperty("LNAME09")
    private String lname09;

    /**
     * DB2 DCLGEN column UTYPE09 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "utype09")
    @JsonProperty("UTYPE09")
    private String utype09;

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
     * DB2 DCLGEN column USRID10 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "usrid10")
    @JsonProperty("USRID10")
    private String usrid10;

    /**
     * DB2 DCLGEN column FNAME10 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "fname10")
    @JsonProperty("FNAME10")
    private String fname10;

    /**
     * DB2 DCLGEN column LNAME10 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "lname10")
    @JsonProperty("LNAME10")
    private String lname10;

    /**
     * DB2 DCLGEN column UTYPE10 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "utype10")
    @JsonProperty("UTYPE10")
    private String utype10;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf29")
    @JsonProperty("DFHMDF29")
    private String dfhmdf29;

    /**
     * DB2 DCLGEN column FROM THE
     * PIC clause: X(50)
     */
    @Column(name = "from")
    @JsonProperty("FROM")
    private String from;

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
