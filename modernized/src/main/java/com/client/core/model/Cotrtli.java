package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: COTRTLI
 * @citation app/app-transaction-type-db2/bms/COTRTLI.bms
 * 84 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "cotrtli")
public class Cotrtli implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * DB2 DCLGEN column COTRTLI DFHMSD
     * PIC clause: X(50)
     */
    @Id
    @Column(name = "cotrtli", nullable = false)
    @JsonProperty("COTRTLI")
    private String cotrtli;

    /**
     * DB2 DCLGEN column CTRTLIA DFHMDI
     * PIC clause: X(50)
     */
    @Column(name = "ctrtlia")
    @JsonProperty("CTRTLIA")
    private String ctrtlia;

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
     * DB2 DCLGEN column TRTYPE DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trtype")
    @JsonProperty("TRTYPE")
    private String trtype;

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
     * DB2 DCLGEN column TRDESC DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trdesc")
    @JsonProperty("TRDESC")
    private String trdesc;

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
     * DB2 DCLGEN column TRTSEL1 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trtsel1")
    @JsonProperty("TRTSEL1")
    private String trtsel1;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf17")
    @JsonProperty("DFHMDF17")
    private String dfhmdf17;

    /**
     * DB2 DCLGEN column TRTTYP1 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trttyp1")
    @JsonProperty("TRTTYP1")
    private String trttyp1;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf18")
    @JsonProperty("DFHMDF18")
    private String dfhmdf18;

    /**
     * DB2 DCLGEN column TRTYPD1 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trtypd1")
    @JsonProperty("TRTYPD1")
    private String trtypd1;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf19")
    @JsonProperty("DFHMDF19")
    private String dfhmdf19;

    /**
     * DB2 DCLGEN column TRTSEL2 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trtsel2")
    @JsonProperty("TRTSEL2")
    private String trtsel2;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf20")
    @JsonProperty("DFHMDF20")
    private String dfhmdf20;

    /**
     * DB2 DCLGEN column TRTTYP2 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trttyp2")
    @JsonProperty("TRTTYP2")
    private String trttyp2;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf21")
    @JsonProperty("DFHMDF21")
    private String dfhmdf21;

    /**
     * DB2 DCLGEN column TRTYPD2 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trtypd2")
    @JsonProperty("TRTYPD2")
    private String trtypd2;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf22")
    @JsonProperty("DFHMDF22")
    private String dfhmdf22;

    /**
     * DB2 DCLGEN column TRTSEL3 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trtsel3")
    @JsonProperty("TRTSEL3")
    private String trtsel3;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf23")
    @JsonProperty("DFHMDF23")
    private String dfhmdf23;

    /**
     * DB2 DCLGEN column TRTTYP3 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trttyp3")
    @JsonProperty("TRTTYP3")
    private String trttyp3;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf24")
    @JsonProperty("DFHMDF24")
    private String dfhmdf24;

    /**
     * DB2 DCLGEN column TRTYPD3 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trtypd3")
    @JsonProperty("TRTYPD3")
    private String trtypd3;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf25")
    @JsonProperty("DFHMDF25")
    private String dfhmdf25;

    /**
     * DB2 DCLGEN column TRTSEL4 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trtsel4")
    @JsonProperty("TRTSEL4")
    private String trtsel4;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf26")
    @JsonProperty("DFHMDF26")
    private String dfhmdf26;

    /**
     * DB2 DCLGEN column TRTTYP4 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trttyp4")
    @JsonProperty("TRTTYP4")
    private String trttyp4;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf27")
    @JsonProperty("DFHMDF27")
    private String dfhmdf27;

    /**
     * DB2 DCLGEN column TRTYPD4 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trtypd4")
    @JsonProperty("TRTYPD4")
    private String trtypd4;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf28")
    @JsonProperty("DFHMDF28")
    private String dfhmdf28;

    /**
     * DB2 DCLGEN column TRTSEL5 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trtsel5")
    @JsonProperty("TRTSEL5")
    private String trtsel5;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf29")
    @JsonProperty("DFHMDF29")
    private String dfhmdf29;

    /**
     * DB2 DCLGEN column TRTTYP5 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trttyp5")
    @JsonProperty("TRTTYP5")
    private String trttyp5;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf30")
    @JsonProperty("DFHMDF30")
    private String dfhmdf30;

    /**
     * DB2 DCLGEN column TRTYPD5 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trtypd5")
    @JsonProperty("TRTYPD5")
    private String trtypd5;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf31")
    @JsonProperty("DFHMDF31")
    private String dfhmdf31;

    /**
     * DB2 DCLGEN column TRTSEL6 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trtsel6")
    @JsonProperty("TRTSEL6")
    private String trtsel6;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf32")
    @JsonProperty("DFHMDF32")
    private String dfhmdf32;

    /**
     * DB2 DCLGEN column TRTTYP6 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trttyp6")
    @JsonProperty("TRTTYP6")
    private String trttyp6;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf33")
    @JsonProperty("DFHMDF33")
    private String dfhmdf33;

    /**
     * DB2 DCLGEN column TRTYPD6 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trtypd6")
    @JsonProperty("TRTYPD6")
    private String trtypd6;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf34")
    @JsonProperty("DFHMDF34")
    private String dfhmdf34;

    /**
     * DB2 DCLGEN column TRTSEL7 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trtsel7")
    @JsonProperty("TRTSEL7")
    private String trtsel7;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf35")
    @JsonProperty("DFHMDF35")
    private String dfhmdf35;

    /**
     * DB2 DCLGEN column TRTTYP7 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trttyp7")
    @JsonProperty("TRTTYP7")
    private String trttyp7;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf36")
    @JsonProperty("DFHMDF36")
    private String dfhmdf36;

    /**
     * DB2 DCLGEN column TRTYPD7 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trtypd7")
    @JsonProperty("TRTYPD7")
    private String trtypd7;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf37")
    @JsonProperty("DFHMDF37")
    private String dfhmdf37;

    /**
     * DB2 DCLGEN column TRTSELA DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trtsela")
    @JsonProperty("TRTSELA")
    private String trtsela;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf38")
    @JsonProperty("DFHMDF38")
    private String dfhmdf38;

    /**
     * DB2 DCLGEN column TRTTYPA DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trttypa")
    @JsonProperty("TRTTYPA")
    private String trttypa;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf39")
    @JsonProperty("DFHMDF39")
    private String dfhmdf39;

    /**
     * DB2 DCLGEN column TRTDSCA DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "trtdsca")
    @JsonProperty("TRTDSCA")
    private String trtdsca;

    /**
     * DB2 DCLGEN column DFHMDF LENGTH
     * PIC clause: X(50)
     */
    @Column(name = "dfhmdf40")
    @JsonProperty("DFHMDF40")
    private String dfhmdf40;

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
    @Column(name = "dfhmdf41")
    @JsonProperty("DFHMDF41")
    private String dfhmdf41;

    /**
     * DB2 DCLGEN column ERRMSG DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "errmsg")
    @JsonProperty("ERRMSG")
    private String errmsg;

    /**
     * DB2 DCLGEN column BUTNF02 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "butnf02")
    @JsonProperty("BUTNF02")
    private String butnf02;

    /**
     * DB2 DCLGEN column BUTNF03 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "butnf03")
    @JsonProperty("BUTNF03")
    private String butnf03;

    /**
     * DB2 DCLGEN column BUTNF07 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "butnf07")
    @JsonProperty("BUTNF07")
    private String butnf07;

    /**
     * DB2 DCLGEN column BUTNF08 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "butnf08")
    @JsonProperty("BUTNF08")
    private String butnf08;

    /**
     * DB2 DCLGEN column BUTNF10 DFHMDF
     * PIC clause: X(50)
     */
    @Column(name = "butnf10")
    @JsonProperty("BUTNF10")
    private String butnf10;

    /**
     * DB2 DCLGEN column DFHMSD TYPE
     * PIC clause: X(50)
     */
    @Column(name = "dfhmsd")
    @JsonProperty("DFHMSD")
    private String dfhmsd;

}
