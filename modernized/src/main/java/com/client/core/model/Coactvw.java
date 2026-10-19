package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: COACTVW
 * @citation app/bms/COACTVW.bms
 * 103 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "coactvw")
public class Coactvw implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * DB2 DCLGEN column COACTVW DFHMSD
     * PIC clause: X(50)
     */
    @Column(name = "coactvw")
    @JsonProperty("COACTVW")
    private String coactvw;

    /**
     * DB2 DCLGEN column CACTVWA DFHMDI
     * PIC clause: X(50)
     */
    @Column(name = "cactvwa")
    @JsonProperty("CACTVWA")
    private String cactvwa;

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
     * DB2 DCLGEN column ACCTSID DFHMDF
     * PIC clause: X(50)
     */
    @Id
    @Column(name = "acctsid", nullable = false)
    @JsonProperty("ACCTSID")
    private String acctsid;

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
     * DB2 DCLGEN column ACSTTUS DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acsttus")
    @JsonProperty("ACSTTUS")
    private String acsttus;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf9")
    @JsonProperty("DFHMDF9")
    private String dfhmdf9;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf10")
    @JsonProperty("DFHMDF10")
    private String dfhmdf10;

    /**
     * DB2 DCLGEN column ADTOPEN DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "adtopen")
    @JsonProperty("ADTOPEN")
    private String adtopen;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
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
     * DB2 DCLGEN column ACRDLIM DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acrdlim")
    @JsonProperty("ACRDLIM")
    private String acrdlim;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf13")
    @JsonProperty("DFHMDF13")
    private String dfhmdf13;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf14")
    @JsonProperty("DFHMDF14")
    private String dfhmdf14;

    /**
     * DB2 DCLGEN column AEXPDT DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "aexpdt")
    @JsonProperty("AEXPDT")
    private String aexpdt;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
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
     * DB2 DCLGEN column ACSHLIM DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acshlim")
    @JsonProperty("ACSHLIM")
    private String acshlim;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf17")
    @JsonProperty("DFHMDF17")
    private String dfhmdf17;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf18")
    @JsonProperty("DFHMDF18")
    private String dfhmdf18;

    /**
     * DB2 DCLGEN column AREISDT DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "areisdt")
    @JsonProperty("AREISDT")
    private String areisdt;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf19")
    @JsonProperty("DFHMDF19")
    private String dfhmdf19;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf20")
    @JsonProperty("DFHMDF20")
    private String dfhmdf20;

    /**
     * DB2 DCLGEN column ACURBAL DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acurbal")
    @JsonProperty("ACURBAL")
    private String acurbal;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf21")
    @JsonProperty("DFHMDF21")
    private String dfhmdf21;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf22")
    @JsonProperty("DFHMDF22")
    private String dfhmdf22;

    /**
     * DB2 DCLGEN column ACRCYCR DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acrcycr")
    @JsonProperty("ACRCYCR")
    private String acrcycr;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf23")
    @JsonProperty("DFHMDF23")
    private String dfhmdf23;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf24")
    @JsonProperty("DFHMDF24")
    private String dfhmdf24;

    /**
     * DB2 DCLGEN column AADDGRP DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "aaddgrp")
    @JsonProperty("AADDGRP")
    private String aaddgrp;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf25")
    @JsonProperty("DFHMDF25")
    private String dfhmdf25;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf26")
    @JsonProperty("DFHMDF26")
    private String dfhmdf26;

    /**
     * DB2 DCLGEN column ACRCYDB DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acrcydb")
    @JsonProperty("ACRCYDB")
    private String acrcydb;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf27")
    @JsonProperty("DFHMDF27")
    private String dfhmdf27;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf28")
    @JsonProperty("DFHMDF28")
    private String dfhmdf28;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf29")
    @JsonProperty("DFHMDF29")
    private String dfhmdf29;

    /**
     * DB2 DCLGEN column ACSTNUM DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acstnum")
    @JsonProperty("ACSTNUM")
    private String acstnum;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf30")
    @JsonProperty("DFHMDF30")
    private String dfhmdf30;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf31")
    @JsonProperty("DFHMDF31")
    private String dfhmdf31;

    /**
     * DB2 DCLGEN column ACSTSSN DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acstssn")
    @JsonProperty("ACSTSSN")
    private String acstssn;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf32")
    @JsonProperty("DFHMDF32")
    private String dfhmdf32;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf33")
    @JsonProperty("DFHMDF33")
    private String dfhmdf33;

    /**
     * DB2 DCLGEN column ACSTDOB DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acstdob")
    @JsonProperty("ACSTDOB")
    private String acstdob;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf34")
    @JsonProperty("DFHMDF34")
    private String dfhmdf34;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf35")
    @JsonProperty("DFHMDF35")
    private String dfhmdf35;

    /**
     * DB2 DCLGEN column ACSTFCO DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acstfco")
    @JsonProperty("ACSTFCO")
    private String acstfco;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf36")
    @JsonProperty("DFHMDF36")
    private String dfhmdf36;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf37")
    @JsonProperty("DFHMDF37")
    private String dfhmdf37;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf38")
    @JsonProperty("DFHMDF38")
    private String dfhmdf38;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf39")
    @JsonProperty("DFHMDF39")
    private String dfhmdf39;

    /**
     * DB2 DCLGEN column ACSFNAM DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acsfnam")
    @JsonProperty("ACSFNAM")
    private String acsfnam;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf40")
    @JsonProperty("DFHMDF40")
    private String dfhmdf40;

    /**
     * DB2 DCLGEN column ACSMNAM DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acsmnam")
    @JsonProperty("ACSMNAM")
    private String acsmnam;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf41")
    @JsonProperty("DFHMDF41")
    private String dfhmdf41;

    /**
     * DB2 DCLGEN column ACSLNAM DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acslnam")
    @JsonProperty("ACSLNAM")
    private String acslnam;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf42")
    @JsonProperty("DFHMDF42")
    private String dfhmdf42;

    /**
     * DB2 DCLGEN column ACSADL1 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acsadl1")
    @JsonProperty("ACSADL1")
    private String acsadl1;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf43")
    @JsonProperty("DFHMDF43")
    private String dfhmdf43;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf44")
    @JsonProperty("DFHMDF44")
    private String dfhmdf44;

    /**
     * DB2 DCLGEN column ACSSTTE DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acsstte")
    @JsonProperty("ACSSTTE")
    private String acsstte;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf45")
    @JsonProperty("DFHMDF45")
    private String dfhmdf45;

    /**
     * DB2 DCLGEN column ACSADL2 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acsadl2")
    @JsonProperty("ACSADL2")
    private String acsadl2;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf46")
    @JsonProperty("DFHMDF46")
    private String dfhmdf46;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf47")
    @JsonProperty("DFHMDF47")
    private String dfhmdf47;

    /**
     * DB2 DCLGEN column ACSZIPC DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acszipc")
    @JsonProperty("ACSZIPC")
    private String acszipc;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf48")
    @JsonProperty("DFHMDF48")
    private String dfhmdf48;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf49")
    @JsonProperty("DFHMDF49")
    private String dfhmdf49;

    /**
     * DB2 DCLGEN column ACSCITY DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acscity")
    @JsonProperty("ACSCITY")
    private String acscity;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf50")
    @JsonProperty("DFHMDF50")
    private String dfhmdf50;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf51")
    @JsonProperty("DFHMDF51")
    private String dfhmdf51;

    /**
     * DB2 DCLGEN column ACSCTRY DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acsctry")
    @JsonProperty("ACSCTRY")
    private String acsctry;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf52")
    @JsonProperty("DFHMDF52")
    private String dfhmdf52;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf53")
    @JsonProperty("DFHMDF53")
    private String dfhmdf53;

    /**
     * DB2 DCLGEN column ACSPHN1 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acsphn1")
    @JsonProperty("ACSPHN1")
    private String acsphn1;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf54")
    @JsonProperty("DFHMDF54")
    private String dfhmdf54;

    /**
     * DB2 DCLGEN column ACSGOVT DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acsgovt")
    @JsonProperty("ACSGOVT")
    private String acsgovt;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf55")
    @JsonProperty("DFHMDF55")
    private String dfhmdf55;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf56")
    @JsonProperty("DFHMDF56")
    private String dfhmdf56;

    /**
     * DB2 DCLGEN column ACSPHN2 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acsphn2")
    @JsonProperty("ACSPHN2")
    private String acsphn2;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf57")
    @JsonProperty("DFHMDF57")
    private String dfhmdf57;

    /**
     * DB2 DCLGEN column ACSEFTC DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acseftc")
    @JsonProperty("ACSEFTC")
    private String acseftc;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf58")
    @JsonProperty("DFHMDF58")
    private String dfhmdf58;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf59")
    @JsonProperty("DFHMDF59")
    private String dfhmdf59;

    /**
     * DB2 DCLGEN column ACSPFLG DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acspflg")
    @JsonProperty("ACSPFLG")
    private String acspflg;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf60")
    @JsonProperty("DFHMDF60")
    private String dfhmdf60;

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
    @Column(name = "dfhmdf61")
    @JsonProperty("DFHMDF61")
    private String dfhmdf61;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf62")
    @JsonProperty("DFHMDF62")
    private String dfhmdf62;

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
    @Column(name = "dfhmdf63")
    @JsonProperty("DFHMDF63")
    private String dfhmdf63;

    /**
     * DB2 DCLGEN column DFHMSD TYPE
     * PIC clause: X(50)
     */
    @Column(name = "dfhmsd")
    @JsonProperty("DFHMSD")
    private String dfhmsd;

}
