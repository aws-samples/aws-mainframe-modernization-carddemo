package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: COADM02Y
 * @citation app/cpy/COADM02Y.cpy
 * 28 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "coadm02y")
public class Coadm02y implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: CDEMO-ADMIN-OPT-COUNT 9(02) (line 22)
     * PIC clause: 9(02)
     */
    @Column(name = "cdemo_admin_opt_count")
    @JsonProperty("CDEMO_ADMIN_OPT_COUNT")
    private Integer cdemoAdminOptCount;

    /**
     * Original: CDEMO-ADMIN-OPT-NUM 9(02) (line 57)
     * PIC clause: 9(02)
     */
    @Id
    @Column(name = "cdemo_admin_opt_num", nullable = false)
    @JsonProperty("CDEMO_ADMIN_OPT_NUM")
    private Integer cdemoAdminOptNum;

    /**
     * Original: CDEMO-ADMIN-OPT-NUM 9(02) occurrence 2 (line 57)
     * PIC clause: 9(02)
     */
    @Column(name = "cdemo_admin_opt_num_2")
    @JsonProperty("CDEMO_ADMIN_OPT_NUM_2")
    private Integer cdemoAdminOptNum2;

    /**
     * Original: CDEMO-ADMIN-OPT-NUM 9(02) occurrence 3 (line 57)
     * PIC clause: 9(02)
     */
    @Column(name = "cdemo_admin_opt_num_3")
    @JsonProperty("CDEMO_ADMIN_OPT_NUM_3")
    private Integer cdemoAdminOptNum3;

    /**
     * Original: CDEMO-ADMIN-OPT-NUM 9(02) occurrence 4 (line 57)
     * PIC clause: 9(02)
     */
    @Column(name = "cdemo_admin_opt_num_4")
    @JsonProperty("CDEMO_ADMIN_OPT_NUM_4")
    private Integer cdemoAdminOptNum4;

    /**
     * Original: CDEMO-ADMIN-OPT-NUM 9(02) occurrence 5 (line 57)
     * PIC clause: 9(02)
     */
    @Column(name = "cdemo_admin_opt_num_5")
    @JsonProperty("CDEMO_ADMIN_OPT_NUM_5")
    private Integer cdemoAdminOptNum5;

    /**
     * Original: CDEMO-ADMIN-OPT-NUM 9(02) occurrence 6 (line 57)
     * PIC clause: 9(02)
     */
    @Column(name = "cdemo_admin_opt_num_6")
    @JsonProperty("CDEMO_ADMIN_OPT_NUM_6")
    private Integer cdemoAdminOptNum6;

    /**
     * Original: CDEMO-ADMIN-OPT-NUM 9(02) occurrence 7 (line 57)
     * PIC clause: 9(02)
     */
    @Column(name = "cdemo_admin_opt_num_7")
    @JsonProperty("CDEMO_ADMIN_OPT_NUM_7")
    private Integer cdemoAdminOptNum7;

    /**
     * Original: CDEMO-ADMIN-OPT-NUM 9(02) occurrence 8 (line 57)
     * PIC clause: 9(02)
     */
    @Column(name = "cdemo_admin_opt_num_8")
    @JsonProperty("CDEMO_ADMIN_OPT_NUM_8")
    private Integer cdemoAdminOptNum8;

    /**
     * Original: CDEMO-ADMIN-OPT-NUM 9(02) occurrence 9 (line 57)
     * PIC clause: 9(02)
     */
    @Column(name = "cdemo_admin_opt_num_9")
    @JsonProperty("CDEMO_ADMIN_OPT_NUM_9")
    private Integer cdemoAdminOptNum9;

    /**
     * Original: CDEMO-ADMIN-OPT-NAME X(35) (line 58)
     * PIC clause: X(35)
     */
    @Column(name = "cdemo_admin_opt_name")
    @JsonProperty("CDEMO_ADMIN_OPT_NAME")
    private String cdemoAdminOptName;

    /**
     * Original: CDEMO-ADMIN-OPT-NAME X(35) occurrence 2 (line 58)
     * PIC clause: X(35)
     */
    @Column(name = "cdemo_admin_opt_name_2")
    @JsonProperty("CDEMO_ADMIN_OPT_NAME_2")
    private String cdemoAdminOptName2;

    /**
     * Original: CDEMO-ADMIN-OPT-NAME X(35) occurrence 3 (line 58)
     * PIC clause: X(35)
     */
    @Column(name = "cdemo_admin_opt_name_3")
    @JsonProperty("CDEMO_ADMIN_OPT_NAME_3")
    private String cdemoAdminOptName3;

    /**
     * Original: CDEMO-ADMIN-OPT-NAME X(35) occurrence 4 (line 58)
     * PIC clause: X(35)
     */
    @Column(name = "cdemo_admin_opt_name_4")
    @JsonProperty("CDEMO_ADMIN_OPT_NAME_4")
    private String cdemoAdminOptName4;

    /**
     * Original: CDEMO-ADMIN-OPT-NAME X(35) occurrence 5 (line 58)
     * PIC clause: X(35)
     */
    @Column(name = "cdemo_admin_opt_name_5")
    @JsonProperty("CDEMO_ADMIN_OPT_NAME_5")
    private String cdemoAdminOptName5;

    /**
     * Original: CDEMO-ADMIN-OPT-NAME X(35) occurrence 6 (line 58)
     * PIC clause: X(35)
     */
    @Column(name = "cdemo_admin_opt_name_6")
    @JsonProperty("CDEMO_ADMIN_OPT_NAME_6")
    private String cdemoAdminOptName6;

    /**
     * Original: CDEMO-ADMIN-OPT-NAME X(35) occurrence 7 (line 58)
     * PIC clause: X(35)
     */
    @Column(name = "cdemo_admin_opt_name_7")
    @JsonProperty("CDEMO_ADMIN_OPT_NAME_7")
    private String cdemoAdminOptName7;

    /**
     * Original: CDEMO-ADMIN-OPT-NAME X(35) occurrence 8 (line 58)
     * PIC clause: X(35)
     */
    @Column(name = "cdemo_admin_opt_name_8")
    @JsonProperty("CDEMO_ADMIN_OPT_NAME_8")
    private String cdemoAdminOptName8;

    /**
     * Original: CDEMO-ADMIN-OPT-NAME X(35) occurrence 9 (line 58)
     * PIC clause: X(35)
     */
    @Column(name = "cdemo_admin_opt_name_9")
    @JsonProperty("CDEMO_ADMIN_OPT_NAME_9")
    private String cdemoAdminOptName9;

    /**
     * Original: CDEMO-ADMIN-OPT-PGMNAME X(08) (line 59)
     * PIC clause: X(08)
     */
    @Column(name = "cdemo_admin_opt_pgmname")
    @JsonProperty("CDEMO_ADMIN_OPT_PGMNAME")
    private String cdemoAdminOptPgmname;

    /**
     * Original: CDEMO-ADMIN-OPT-PGMNAME X(08) occurrence 2 (line 59)
     * PIC clause: X(08)
     */
    @Column(name = "cdemo_admin_opt_pgmname_2")
    @JsonProperty("CDEMO_ADMIN_OPT_PGMNAME_2")
    private String cdemoAdminOptPgmname2;

    /**
     * Original: CDEMO-ADMIN-OPT-PGMNAME X(08) occurrence 3 (line 59)
     * PIC clause: X(08)
     */
    @Column(name = "cdemo_admin_opt_pgmname_3")
    @JsonProperty("CDEMO_ADMIN_OPT_PGMNAME_3")
    private String cdemoAdminOptPgmname3;

    /**
     * Original: CDEMO-ADMIN-OPT-PGMNAME X(08) occurrence 4 (line 59)
     * PIC clause: X(08)
     */
    @Column(name = "cdemo_admin_opt_pgmname_4")
    @JsonProperty("CDEMO_ADMIN_OPT_PGMNAME_4")
    private String cdemoAdminOptPgmname4;

    /**
     * Original: CDEMO-ADMIN-OPT-PGMNAME X(08) occurrence 5 (line 59)
     * PIC clause: X(08)
     */
    @Column(name = "cdemo_admin_opt_pgmname_5")
    @JsonProperty("CDEMO_ADMIN_OPT_PGMNAME_5")
    private String cdemoAdminOptPgmname5;

    /**
     * Original: CDEMO-ADMIN-OPT-PGMNAME X(08) occurrence 6 (line 59)
     * PIC clause: X(08)
     */
    @Column(name = "cdemo_admin_opt_pgmname_6")
    @JsonProperty("CDEMO_ADMIN_OPT_PGMNAME_6")
    private String cdemoAdminOptPgmname6;

    /**
     * Original: CDEMO-ADMIN-OPT-PGMNAME X(08) occurrence 7 (line 59)
     * PIC clause: X(08)
     */
    @Column(name = "cdemo_admin_opt_pgmname_7")
    @JsonProperty("CDEMO_ADMIN_OPT_PGMNAME_7")
    private String cdemoAdminOptPgmname7;

    /**
     * Original: CDEMO-ADMIN-OPT-PGMNAME X(08) occurrence 8 (line 59)
     * PIC clause: X(08)
     */
    @Column(name = "cdemo_admin_opt_pgmname_8")
    @JsonProperty("CDEMO_ADMIN_OPT_PGMNAME_8")
    private String cdemoAdminOptPgmname8;

    /**
     * Original: CDEMO-ADMIN-OPT-PGMNAME X(08) occurrence 9 (line 59)
     * PIC clause: X(08)
     */
    @Column(name = "cdemo_admin_opt_pgmname_9")
    @JsonProperty("CDEMO_ADMIN_OPT_PGMNAME_9")
    private String cdemoAdminOptPgmname9;

}
