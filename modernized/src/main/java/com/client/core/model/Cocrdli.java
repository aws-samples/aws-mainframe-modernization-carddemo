package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: COCRDLI
 * @citation app/bms/COCRDLI.bms
 * 75 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "cocrdli")
public class Cocrdli implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * DB2 DCLGEN column COCRDLI DFHMSD
     * PIC clause: X(50)
     */
    @Column(name = "cocrdli")
    @JsonProperty("COCRDLI")
    private String cocrdli;

    /**
     * DB2 DCLGEN column CCRDLIA DFHMDI
     * PIC clause: X(50)
     */
    @Column(name = "ccrdlia")
    @JsonProperty("CCRDLIA")
    private String ccrdlia;

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
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf6")
    @JsonProperty("DFHMDF6")
    private String dfhmdf6;

    /**
     * DB2 DCLGEN column PAGENO DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "pageno")
    @JsonProperty("PAGENO")
    private String pageno;

    /**
     * DB2 DCLGEN column DFHMDF ATTRB
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf7")
    @JsonProperty("DFHMDF7")
    private String dfhmdf7;

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
     * DB2 DCLGEN column CARDSID DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "cardsid")
    @JsonProperty("CARDSID")
    private String cardsid;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf10")
    @JsonProperty("DFHMDF10")
    private String dfhmdf10;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf11")
    @JsonProperty("DFHMDF11")
    private String dfhmdf11;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf12")
    @JsonProperty("DFHMDF12")
    private String dfhmdf12;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
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
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf15")
    @JsonProperty("DFHMDF15")
    private String dfhmdf15;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf16")
    @JsonProperty("DFHMDF16")
    private String dfhmdf16;

    /**
     * DB2 DCLGEN column DFHMDF COLOR
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
     * DB2 DCLGEN column CRDSEL1 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdsel1")
    @JsonProperty("CRDSEL1")
    private String crdsel1;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf19")
    @JsonProperty("DFHMDF19")
    private String dfhmdf19;

    /**
     * DB2 DCLGEN column ACCTNO1 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acctno1")
    @JsonProperty("ACCTNO1")
    private String acctno1;

    /**
     * DB2 DCLGEN column CRDNUM1 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdnum1")
    @JsonProperty("CRDNUM1")
    private String crdnum1;

    /**
     * DB2 DCLGEN column CRDSTS1 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdsts1")
    @JsonProperty("CRDSTS1")
    private String crdsts1;

    /**
     * DB2 DCLGEN column CRDSEL2 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdsel2")
    @JsonProperty("CRDSEL2")
    private String crdsel2;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf20")
    @JsonProperty("DFHMDF20")
    private String dfhmdf20;

    /**
     * DB2 DCLGEN column CRDSTP2 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdstp2")
    @JsonProperty("CRDSTP2")
    private String crdstp2;

    /**
     * DB2 DCLGEN column ACCTNO2 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acctno2")
    @JsonProperty("ACCTNO2")
    private String acctno2;

    /**
     * DB2 DCLGEN column CRDNUM2 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdnum2")
    @JsonProperty("CRDNUM2")
    private String crdnum2;

    /**
     * DB2 DCLGEN column CRDSTS2 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdsts2")
    @JsonProperty("CRDSTS2")
    private String crdsts2;

    /**
     * DB2 DCLGEN column CRDSEL3 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdsel3")
    @JsonProperty("CRDSEL3")
    private String crdsel3;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf21")
    @JsonProperty("DFHMDF21")
    private String dfhmdf21;

    /**
     * DB2 DCLGEN column CRDSTP3 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdstp3")
    @JsonProperty("CRDSTP3")
    private String crdstp3;

    /**
     * DB2 DCLGEN column ACCTNO3 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acctno3")
    @JsonProperty("ACCTNO3")
    private String acctno3;

    /**
     * DB2 DCLGEN column CRDNUM3 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdnum3")
    @JsonProperty("CRDNUM3")
    private String crdnum3;

    /**
     * DB2 DCLGEN column CRDSTS3 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdsts3")
    @JsonProperty("CRDSTS3")
    private String crdsts3;

    /**
     * DB2 DCLGEN column CRDSEL4 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdsel4")
    @JsonProperty("CRDSEL4")
    private String crdsel4;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf22")
    @JsonProperty("DFHMDF22")
    private String dfhmdf22;

    /**
     * DB2 DCLGEN column CRDSTP4 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdstp4")
    @JsonProperty("CRDSTP4")
    private String crdstp4;

    /**
     * DB2 DCLGEN column ACCTNO4 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acctno4")
    @JsonProperty("ACCTNO4")
    private String acctno4;

    /**
     * DB2 DCLGEN column CRDNUM4 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdnum4")
    @JsonProperty("CRDNUM4")
    private String crdnum4;

    /**
     * DB2 DCLGEN column CRDSTS4 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdsts4")
    @JsonProperty("CRDSTS4")
    private String crdsts4;

    /**
     * DB2 DCLGEN column CRDSEL5 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdsel5")
    @JsonProperty("CRDSEL5")
    private String crdsel5;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf23")
    @JsonProperty("DFHMDF23")
    private String dfhmdf23;

    /**
     * DB2 DCLGEN column CRDSTP5 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdstp5")
    @JsonProperty("CRDSTP5")
    private String crdstp5;

    /**
     * DB2 DCLGEN column ACCTNO5 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acctno5")
    @JsonProperty("ACCTNO5")
    private String acctno5;

    /**
     * DB2 DCLGEN column CRDNUM5 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdnum5")
    @JsonProperty("CRDNUM5")
    private String crdnum5;

    /**
     * DB2 DCLGEN column CRDSTS5 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdsts5")
    @JsonProperty("CRDSTS5")
    private String crdsts5;

    /**
     * DB2 DCLGEN column CRDSEL6 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdsel6")
    @JsonProperty("CRDSEL6")
    private String crdsel6;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf24")
    @JsonProperty("DFHMDF24")
    private String dfhmdf24;

    /**
     * DB2 DCLGEN column CRDSTP6 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdstp6")
    @JsonProperty("CRDSTP6")
    private String crdstp6;

    /**
     * DB2 DCLGEN column ACCTNO6 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acctno6")
    @JsonProperty("ACCTNO6")
    private String acctno6;

    /**
     * DB2 DCLGEN column CRDNUM6 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdnum6")
    @JsonProperty("CRDNUM6")
    private String crdnum6;

    /**
     * DB2 DCLGEN column CRDSTS6 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdsts6")
    @JsonProperty("CRDSTS6")
    private String crdsts6;

    /**
     * DB2 DCLGEN column CRDSEL7 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdsel7")
    @JsonProperty("CRDSEL7")
    private String crdsel7;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf25")
    @JsonProperty("DFHMDF25")
    private String dfhmdf25;

    /**
     * DB2 DCLGEN column CRDSTP7 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdstp7")
    @JsonProperty("CRDSTP7")
    private String crdstp7;

    /**
     * DB2 DCLGEN column ACCTNO7 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "acctno7")
    @JsonProperty("ACCTNO7")
    private String acctno7;

    /**
     * DB2 DCLGEN column CRDNUM7 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdnum7")
    @JsonProperty("CRDNUM7")
    private String crdnum7;

    /**
     * DB2 DCLGEN column CRDSTS7 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "crdsts7")
    @JsonProperty("CRDSTS7")
    private String crdsts7;

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
    @Column(name = "dfhmdf26")
    @JsonProperty("DFHMDF26")
    private String dfhmdf26;

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
    @Column(name = "dfhmdf27")
    @JsonProperty("DFHMDF27")
    private String dfhmdf27;

    /**
     * DB2 DCLGEN column DFHMSD TYPE
     * PIC clause: X(50)
     */
    @Column(name = "dfhmsd")
    @JsonProperty("DFHMSD")
    private String dfhmsd;

}
