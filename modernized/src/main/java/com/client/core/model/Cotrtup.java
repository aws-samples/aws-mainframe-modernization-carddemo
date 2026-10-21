package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: COTRTUP
 * @citation app/app-transaction-type-db2/bms/COTRTUP.bms
 * 28 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "cotrtup")
public class Cotrtup implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * DB2 DCLGEN column COTRTUP DFHMSD
     * PIC clause: X(50)
     */
    @Column(name = "cotrtup")
    @JsonProperty("COTRTUP")
    private String cotrtup;

    /**
     * DB2 DCLGEN column CTRTUPA DFHMDI
     * PIC clause: X(50)
     */
    @Column(name = "ctrtupa")
    @JsonProperty("CTRTUPA")
    private String ctrtupa;

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
     * DB2 DCLGEN column DFHMDF COLOR
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
     * DB2 DCLGEN column TRTYPCD DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trtypcd")
    @JsonProperty("TRTYPCD")
    private String trtypcd;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf7")
    @JsonProperty("DFHMDF7")
    private String dfhmdf7;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf8")
    @JsonProperty("DFHMDF8")
    private String dfhmdf8;

    /**
     * DB2 DCLGEN column TRTYDSC DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trtydsc")
    @JsonProperty("TRTYDSC")
    private String trtydsc;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf9")
    @JsonProperty("DFHMDF9")
    private String dfhmdf9;

    /**
     * DB2 DCLGEN column INFOMSG DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "infomsg")
    @JsonProperty("INFOMSG")
    private String infomsg;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf10")
    @JsonProperty("DFHMDF10")
    private String dfhmdf10;

    /**
     * DB2 DCLGEN column ERRMSG DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "errmsg")
    @JsonProperty("ERRMSG")
    private String errmsg;

    /**
     * DB2 DCLGEN column FKEYS DFHMDF
     * PIC clause: X(50)
     */
    @Id
    @Column(name = "fkeys", nullable = false)
    @JsonProperty("FKEYS")
    private String fkeys;

    /**
     * DB2 DCLGEN column FKEY04 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "fkey04")
    @JsonProperty("FKEY04")
    private String fkey04;

    /**
     * DB2 DCLGEN column FKEY05 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "fkey05")
    @JsonProperty("FKEY05")
    private String fkey05;

    /**
     * DB2 DCLGEN column FKEY06 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "fkey06")
    @JsonProperty("FKEY06")
    private String fkey06;

    /**
     * DB2 DCLGEN column FKEY12 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "fkey12")
    @JsonProperty("FKEY12")
    private String fkey12;

    /**
     * DB2 DCLGEN column DFHMSD TYPE
     * PIC clause: X(50)
     */
    @Column(name = "dfhmsd")
    @JsonProperty("DFHMSD")
    private String dfhmsd;

}
