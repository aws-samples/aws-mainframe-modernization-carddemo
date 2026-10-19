package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: COPAU00
 * @citation app/app-authorization-ims-db2-mq/bms/COPAU00.bms
 * 108 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "copau00")
public class Copau00 implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * DB2 DCLGEN column COPAU00 DFHMSD
     * PIC clause: X(50)
     */
    @Column(name = "copau00")
    @JsonProperty("COPAU00")
    private String copau00;

    /**
     * DB2 DCLGEN column COPAU0A DFHMDI
     * PIC clause: X(50)
     */
    @Column(name = "copau0a")
    @JsonProperty("COPAU0A")
    private String copau0a;

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
     * DB2 DCLGEN column ACCTID DFHMDF
     * PIC clause: X(50)
     */
    @Id
    @Column(name = "acctid", nullable = false)
    @JsonProperty("ACCTID")
    private String acctid;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
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
     * DB2 DCLGEN column CNAME DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "cname")
    @JsonProperty("CNAME")
    private String cname;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf9")
    @JsonProperty("DFHMDF9")
    private String dfhmdf9;

    /**
     * DB2 DCLGEN column CUSTID DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "custid")
    @JsonProperty("CUSTID")
    private String custid;

    /**
     * DB2 DCLGEN column ADDR001 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "addr001")
    @JsonProperty("ADDR001")
    private String addr001;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf10")
    @JsonProperty("DFHMDF10")
    private String dfhmdf10;

    /**
     * DB2 DCLGEN column ACCSTAT DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "accstat")
    @JsonProperty("ACCSTAT")
    private String accstat;

    /**
     * DB2 DCLGEN column ADDR002 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "addr002")
    @JsonProperty("ADDR002")
    private String addr002;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf11")
    @JsonProperty("DFHMDF11")
    private String dfhmdf11;

    /**
     * DB2 DCLGEN column PHONE1 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "phone1")
    @JsonProperty("PHONE1")
    private String phone1;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf12")
    @JsonProperty("DFHMDF12")
    private String dfhmdf12;

    /**
     * DB2 DCLGEN column APPRCNT DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "apprcnt")
    @JsonProperty("APPRCNT")
    private String apprcnt;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf13")
    @JsonProperty("DFHMDF13")
    private String dfhmdf13;

    /**
     * DB2 DCLGEN column DECLCNT DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "declcnt")
    @JsonProperty("DECLCNT")
    private String declcnt;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf14")
    @JsonProperty("DFHMDF14")
    private String dfhmdf14;

    /**
     * DB2 DCLGEN column CREDLIM DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "credlim")
    @JsonProperty("CREDLIM")
    private String credlim;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf15")
    @JsonProperty("DFHMDF15")
    private String dfhmdf15;

    /**
     * DB2 DCLGEN column CASHLIM DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "cashlim")
    @JsonProperty("CASHLIM")
    private String cashlim;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf16")
    @JsonProperty("DFHMDF16")
    private String dfhmdf16;

    /**
     * DB2 DCLGEN column APPRAMT DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "appramt")
    @JsonProperty("APPRAMT")
    private String appramt;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf17")
    @JsonProperty("DFHMDF17")
    private String dfhmdf17;

    /**
     * DB2 DCLGEN column CREDBAL DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "credbal")
    @JsonProperty("CREDBAL")
    private String credbal;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf18")
    @JsonProperty("DFHMDF18")
    private String dfhmdf18;

    /**
     * DB2 DCLGEN column CASHBAL DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "cashbal")
    @JsonProperty("CASHBAL")
    private String cashbal;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf19")
    @JsonProperty("DFHMDF19")
    private String dfhmdf19;

    /**
     * DB2 DCLGEN column DECLAMT DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "declamt")
    @JsonProperty("DECLAMT")
    private String declamt;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
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
     * DB2 DCLGEN column DFHMDF ATTRB
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
     * DB2 DCLGEN column DFHMDF ATTRB
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
     * DB2 DCLGEN column DFHMDF ATTRB
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
     * DB2 DCLGEN column DFHMDF ATTRB
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
     * DB2 DCLGEN column DFHMDF ATTRB
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
     * DB2 DCLGEN column DFHMDF ATTRB
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
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf34")
    @JsonProperty("DFHMDF34")
    private String dfhmdf34;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf35")
    @JsonProperty("DFHMDF35")
    private String dfhmdf35;

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
    @Column(name = "dfhmdf36")
    @JsonProperty("DFHMDF36")
    private String dfhmdf36;

    /**
     * DB2 DCLGEN column TRNID01 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trnid01")
    @JsonProperty("TRNID01")
    private String trnid01;

    /**
     * DB2 DCLGEN column PDATE01 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "pdate01")
    @JsonProperty("PDATE01")
    private String pdate01;

    /**
     * DB2 DCLGEN column PTIME01 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "ptime01")
    @JsonProperty("PTIME01")
    private String ptime01;

    /**
     * DB2 DCLGEN column PTYPE01 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "ptype01")
    @JsonProperty("PTYPE01")
    private String ptype01;

    /**
     * DB2 DCLGEN column PAPRV01 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "paprv01")
    @JsonProperty("PAPRV01")
    private String paprv01;

    /**
     * DB2 DCLGEN column PSTAT01 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "pstat01")
    @JsonProperty("PSTAT01")
    private String pstat01;

    /**
     * DB2 DCLGEN column PAMT001 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "pamt001")
    @JsonProperty("PAMT001")
    private String pamt001;

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
    @Column(name = "dfhmdf37")
    @JsonProperty("DFHMDF37")
    private String dfhmdf37;

    /**
     * DB2 DCLGEN column TRNID02 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trnid02")
    @JsonProperty("TRNID02")
    private String trnid02;

    /**
     * DB2 DCLGEN column PDATE02 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "pdate02")
    @JsonProperty("PDATE02")
    private String pdate02;

    /**
     * DB2 DCLGEN column PTIME02 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "ptime02")
    @JsonProperty("PTIME02")
    private String ptime02;

    /**
     * DB2 DCLGEN column PTYPE02 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "ptype02")
    @JsonProperty("PTYPE02")
    private String ptype02;

    /**
     * DB2 DCLGEN column PAPRV02 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "paprv02")
    @JsonProperty("PAPRV02")
    private String paprv02;

    /**
     * DB2 DCLGEN column PSTAT02 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "pstat02")
    @JsonProperty("PSTAT02")
    private String pstat02;

    /**
     * DB2 DCLGEN column PAMT002 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "pamt002")
    @JsonProperty("PAMT002")
    private String pamt002;

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
    @Column(name = "dfhmdf38")
    @JsonProperty("DFHMDF38")
    private String dfhmdf38;

    /**
     * DB2 DCLGEN column TRNID03 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trnid03")
    @JsonProperty("TRNID03")
    private String trnid03;

    /**
     * DB2 DCLGEN column PDATE03 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "pdate03")
    @JsonProperty("PDATE03")
    private String pdate03;

    /**
     * DB2 DCLGEN column PTIME03 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "ptime03")
    @JsonProperty("PTIME03")
    private String ptime03;

    /**
     * DB2 DCLGEN column PTYPE03 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "ptype03")
    @JsonProperty("PTYPE03")
    private String ptype03;

    /**
     * DB2 DCLGEN column PAPRV03 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "paprv03")
    @JsonProperty("PAPRV03")
    private String paprv03;

    /**
     * DB2 DCLGEN column PSTAT03 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "pstat03")
    @JsonProperty("PSTAT03")
    private String pstat03;

    /**
     * DB2 DCLGEN column PAMT003 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "pamt003")
    @JsonProperty("PAMT003")
    private String pamt003;

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
    @Column(name = "dfhmdf39")
    @JsonProperty("DFHMDF39")
    private String dfhmdf39;

    /**
     * DB2 DCLGEN column TRNID04 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trnid04")
    @JsonProperty("TRNID04")
    private String trnid04;

    /**
     * DB2 DCLGEN column PDATE04 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "pdate04")
    @JsonProperty("PDATE04")
    private String pdate04;

    /**
     * DB2 DCLGEN column PTIME04 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "ptime04")
    @JsonProperty("PTIME04")
    private String ptime04;

    /**
     * DB2 DCLGEN column PTYPE04 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "ptype04")
    @JsonProperty("PTYPE04")
    private String ptype04;

    /**
     * DB2 DCLGEN column PAPRV04 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "paprv04")
    @JsonProperty("PAPRV04")
    private String paprv04;

    /**
     * DB2 DCLGEN column PSTAT04 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "pstat04")
    @JsonProperty("PSTAT04")
    private String pstat04;

    /**
     * DB2 DCLGEN column PAMT004 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "pamt004")
    @JsonProperty("PAMT004")
    private String pamt004;

    /**
     * DB2 DCLGEN column TRNID05 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trnid05")
    @JsonProperty("TRNID05")
    private String trnid05;

    /**
     * DB2 DCLGEN column PDATE05 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "pdate05")
    @JsonProperty("PDATE05")
    private String pdate05;

    /**
     * DB2 DCLGEN column PTIME05 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "ptime05")
    @JsonProperty("PTIME05")
    private String ptime05;

    /**
     * DB2 DCLGEN column PTYPE05 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "ptype05")
    @JsonProperty("PTYPE05")
    private String ptype05;

    /**
     * DB2 DCLGEN column PAPRV05 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "paprv05")
    @JsonProperty("PAPRV05")
    private String paprv05;

    /**
     * DB2 DCLGEN column PSTAT05 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "pstat05")
    @JsonProperty("PSTAT05")
    private String pstat05;

    /**
     * DB2 DCLGEN column PAMT005 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "pamt005")
    @JsonProperty("PAMT005")
    private String pamt005;

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
    @Column(name = "dfhmdf40")
    @JsonProperty("DFHMDF40")
    private String dfhmdf40;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf41")
    @JsonProperty("DFHMDF41")
    private String dfhmdf41;

    /**
     * DB2 DCLGEN column HE LIST
     * PIC clause: X(50)
     */
    @Column(name = "he")
    @JsonProperty("HE")
    private String he;

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
    @Column(name = "dfhmdf42")
    @JsonProperty("DFHMDF42")
    private String dfhmdf42;

    /**
     * DB2 DCLGEN column DFHMSD TYPE
     * PIC clause: X(50)
     */
    @Column(name = "dfhmsd")
    @JsonProperty("DFHMSD")
    private String dfhmsd;

}
