package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: CSSTRPFY
 * @citation app/cpy/CSSTRPFY.cpy
 * 57 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "csstrpfy")
public class Csstrpfy implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * DB2 DCLGEN column EVALUATE TRUE
     * PIC clause: X(50)
     */
    @Id
    @Column(name = "evaluate", nullable = false)
    @JsonProperty("EVALUATE")
    private String evaluate;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when")
    @JsonProperty("WHEN")
    private String when;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set")
    @JsonProperty("SET")
    private String set;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when2")
    @JsonProperty("WHEN2")
    private String when2;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set2")
    @JsonProperty("SET2")
    private String set2;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when3")
    @JsonProperty("WHEN3")
    private String when3;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set3")
    @JsonProperty("SET3")
    private String set3;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when4")
    @JsonProperty("WHEN4")
    private String when4;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set4")
    @JsonProperty("SET4")
    private String set4;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when5")
    @JsonProperty("WHEN5")
    private String when5;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set5")
    @JsonProperty("SET5")
    private String set5;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when6")
    @JsonProperty("WHEN6")
    private String when6;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set6")
    @JsonProperty("SET6")
    private String set6;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when7")
    @JsonProperty("WHEN7")
    private String when7;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set7")
    @JsonProperty("SET7")
    private String set7;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when8")
    @JsonProperty("WHEN8")
    private String when8;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set8")
    @JsonProperty("SET8")
    private String set8;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when9")
    @JsonProperty("WHEN9")
    private String when9;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set9")
    @JsonProperty("SET9")
    private String set9;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when10")
    @JsonProperty("WHEN10")
    private String when10;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set10")
    @JsonProperty("SET10")
    private String set10;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when11")
    @JsonProperty("WHEN11")
    private String when11;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set11")
    @JsonProperty("SET11")
    private String set11;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when12")
    @JsonProperty("WHEN12")
    private String when12;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set12")
    @JsonProperty("SET12")
    private String set12;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when13")
    @JsonProperty("WHEN13")
    private String when13;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set13")
    @JsonProperty("SET13")
    private String set13;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when14")
    @JsonProperty("WHEN14")
    private String when14;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set14")
    @JsonProperty("SET14")
    private String set14;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when15")
    @JsonProperty("WHEN15")
    private String when15;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set15")
    @JsonProperty("SET15")
    private String set15;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when16")
    @JsonProperty("WHEN16")
    private String when16;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set16")
    @JsonProperty("SET16")
    private String set16;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when17")
    @JsonProperty("WHEN17")
    private String when17;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set17")
    @JsonProperty("SET17")
    private String set17;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when18")
    @JsonProperty("WHEN18")
    private String when18;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set18")
    @JsonProperty("SET18")
    private String set18;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when19")
    @JsonProperty("WHEN19")
    private String when19;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set19")
    @JsonProperty("SET19")
    private String set19;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when20")
    @JsonProperty("WHEN20")
    private String when20;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set20")
    @JsonProperty("SET20")
    private String set20;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when21")
    @JsonProperty("WHEN21")
    private String when21;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set21")
    @JsonProperty("SET21")
    private String set21;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when22")
    @JsonProperty("WHEN22")
    private String when22;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set22")
    @JsonProperty("SET22")
    private String set22;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when23")
    @JsonProperty("WHEN23")
    private String when23;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set23")
    @JsonProperty("SET23")
    private String set23;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when24")
    @JsonProperty("WHEN24")
    private String when24;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set24")
    @JsonProperty("SET24")
    private String set24;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when25")
    @JsonProperty("WHEN25")
    private String when25;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set25")
    @JsonProperty("SET25")
    private String set25;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when26")
    @JsonProperty("WHEN26")
    private String when26;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set26")
    @JsonProperty("SET26")
    private String set26;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when27")
    @JsonProperty("WHEN27")
    private String when27;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set27")
    @JsonProperty("SET27")
    private String set27;

    /**
     * DB2 DCLGEN column WHEN EIBAID
     * PIC clause: X(50)
     */
    @Column(name = "when28")
    @JsonProperty("WHEN28")
    private String when28;

    /**
     * DB2 DCLGEN column SET CCARD
     * PIC clause: X(50)
     */
    @Column(name = "set28")
    @JsonProperty("SET28")
    private String set28;

}
