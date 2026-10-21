package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: COMEN01
 * @citation app/bms/COMEN01.bms
 * 31 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "comen01")
public class Comen01 implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * DB2 DCLGEN column COMEN01 DFHMSD
     * PIC clause: X(50)
     */
    @Id
    @Column(name = "comen01", nullable = false)
    @JsonProperty("COMEN01")
    private String comen01;

    /**
     * DB2 DCLGEN column COMEN1A DFHMDI
     * PIC clause: X(50)
     */
    @Column(name = "comen1a")
    @JsonProperty("COMEN1A")
    private String comen1a;

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
     * DB2 DCLGEN column OPTN001 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "optn001")
    @JsonProperty("OPTN001")
    private String optn001;

    /**
     * DB2 DCLGEN column OPTN002 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "optn002")
    @JsonProperty("OPTN002")
    private String optn002;

    /**
     * DB2 DCLGEN column OPTN003 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "optn003")
    @JsonProperty("OPTN003")
    private String optn003;

    /**
     * DB2 DCLGEN column OPTN004 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "optn004")
    @JsonProperty("OPTN004")
    private String optn004;

    /**
     * DB2 DCLGEN column OPTN005 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "optn005")
    @JsonProperty("OPTN005")
    private String optn005;

    /**
     * DB2 DCLGEN column OPTN006 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "optn006")
    @JsonProperty("OPTN006")
    private String optn006;

    /**
     * DB2 DCLGEN column OPTN007 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "optn007")
    @JsonProperty("OPTN007")
    private String optn007;

    /**
     * DB2 DCLGEN column OPTN008 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "optn008")
    @JsonProperty("OPTN008")
    private String optn008;

    /**
     * DB2 DCLGEN column OPTN009 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "optn009")
    @JsonProperty("OPTN009")
    private String optn009;

    /**
     * DB2 DCLGEN column OPTN010 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "optn010")
    @JsonProperty("OPTN010")
    private String optn010;

    /**
     * DB2 DCLGEN column OPTN011 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "optn011")
    @JsonProperty("OPTN011")
    private String optn011;

    /**
     * DB2 DCLGEN column OPTN012 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "optn012")
    @JsonProperty("OPTN012")
    private String optn012;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf6")
    @JsonProperty("DFHMDF6")
    private String dfhmdf6;

    /**
     * DB2 DCLGEN column OPTION DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "option")
    @JsonProperty("OPTION")
    private String option;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf7")
    @JsonProperty("DFHMDF7")
    private String dfhmdf7;

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
    @Column(name = "dfhmdf8")
    @JsonProperty("DFHMDF8")
    private String dfhmdf8;

    /**
     * DB2 DCLGEN column DFHMSD TYPE
     * PIC clause: X(50)
     */
    @Column(name = "dfhmsd")
    @JsonProperty("DFHMSD")
    private String dfhmsd;

}
