package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: COMEN02Y
 * @citation app/cpy/COMEN02Y.cpy
 * 49 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "comen02y")
public class Comen02y implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: CDEMO-MENU-OPT-COUNT 9(02) (line 21)
     * PIC clause: 9(02)
     */
    @Column(name = "cdemo_menu_opt_count")
    @JsonProperty("CDEMO_MENU_OPT_COUNT")
    private Integer cdemoMenuOptCount;

    /**
     * Original: CDEMO-MENU-OPT-NUM 9(02) (line 95)
     * PIC clause: 9(02)
     */
    @Id
    @Column(name = "cdemo_menu_opt_num", nullable = false)
    @JsonProperty("CDEMO_MENU_OPT_NUM")
    private Integer cdemoMenuOptNum;

    /**
     * Original: CDEMO-MENU-OPT-NUM 9(02) occurrence 2 (line 95)
     * PIC clause: 9(02)
     */
    @Column(name = "cdemo_menu_opt_num_2")
    @JsonProperty("CDEMO_MENU_OPT_NUM_2")
    private Integer cdemoMenuOptNum2;

    /**
     * Original: CDEMO-MENU-OPT-NUM 9(02) occurrence 3 (line 95)
     * PIC clause: 9(02)
     */
    @Column(name = "cdemo_menu_opt_num_3")
    @JsonProperty("CDEMO_MENU_OPT_NUM_3")
    private Integer cdemoMenuOptNum3;

    /**
     * Original: CDEMO-MENU-OPT-NUM 9(02) occurrence 4 (line 95)
     * PIC clause: 9(02)
     */
    @Column(name = "cdemo_menu_opt_num_4")
    @JsonProperty("CDEMO_MENU_OPT_NUM_4")
    private Integer cdemoMenuOptNum4;

    /**
     * Original: CDEMO-MENU-OPT-NUM 9(02) occurrence 5 (line 95)
     * PIC clause: 9(02)
     */
    @Column(name = "cdemo_menu_opt_num_5")
    @JsonProperty("CDEMO_MENU_OPT_NUM_5")
    private Integer cdemoMenuOptNum5;

    /**
     * Original: CDEMO-MENU-OPT-NUM 9(02) occurrence 6 (line 95)
     * PIC clause: 9(02)
     */
    @Column(name = "cdemo_menu_opt_num_6")
    @JsonProperty("CDEMO_MENU_OPT_NUM_6")
    private Integer cdemoMenuOptNum6;

    /**
     * Original: CDEMO-MENU-OPT-NUM 9(02) occurrence 7 (line 95)
     * PIC clause: 9(02)
     */
    @Column(name = "cdemo_menu_opt_num_7")
    @JsonProperty("CDEMO_MENU_OPT_NUM_7")
    private Integer cdemoMenuOptNum7;

    /**
     * Original: CDEMO-MENU-OPT-NUM 9(02) occurrence 8 (line 95)
     * PIC clause: 9(02)
     */
    @Column(name = "cdemo_menu_opt_num_8")
    @JsonProperty("CDEMO_MENU_OPT_NUM_8")
    private Integer cdemoMenuOptNum8;

    /**
     * Original: CDEMO-MENU-OPT-NUM 9(02) occurrence 9 (line 95)
     * PIC clause: 9(02)
     */
    @Column(name = "cdemo_menu_opt_num_9")
    @JsonProperty("CDEMO_MENU_OPT_NUM_9")
    private Integer cdemoMenuOptNum9;

    /**
     * Original: CDEMO-MENU-OPT-NUM 9(02) occurrence 10 (line 95)
     * PIC clause: 9(02)
     */
    @Column(name = "cdemo_menu_opt_num_10")
    @JsonProperty("CDEMO_MENU_OPT_NUM_10")
    private Integer cdemoMenuOptNum10;

    /**
     * Original: CDEMO-MENU-OPT-NUM 9(02) occurrence 11 (line 95)
     * PIC clause: 9(02)
     */
    @Column(name = "cdemo_menu_opt_num_11")
    @JsonProperty("CDEMO_MENU_OPT_NUM_11")
    private Integer cdemoMenuOptNum11;

    /**
     * Original: CDEMO-MENU-OPT-NUM 9(02) occurrence 12 (line 95)
     * PIC clause: 9(02)
     */
    @Column(name = "cdemo_menu_opt_num_12")
    @JsonProperty("CDEMO_MENU_OPT_NUM_12")
    private Integer cdemoMenuOptNum12;

    /**
     * Original: CDEMO-MENU-OPT-NAME X(35) (line 96)
     * PIC clause: X(35)
     */
    @Column(name = "cdemo_menu_opt_name")
    @JsonProperty("CDEMO_MENU_OPT_NAME")
    private String cdemoMenuOptName;

    /**
     * Original: CDEMO-MENU-OPT-NAME X(35) occurrence 2 (line 96)
     * PIC clause: X(35)
     */
    @Column(name = "cdemo_menu_opt_name_2")
    @JsonProperty("CDEMO_MENU_OPT_NAME_2")
    private String cdemoMenuOptName2;

    /**
     * Original: CDEMO-MENU-OPT-NAME X(35) occurrence 3 (line 96)
     * PIC clause: X(35)
     */
    @Column(name = "cdemo_menu_opt_name_3")
    @JsonProperty("CDEMO_MENU_OPT_NAME_3")
    private String cdemoMenuOptName3;

    /**
     * Original: CDEMO-MENU-OPT-NAME X(35) occurrence 4 (line 96)
     * PIC clause: X(35)
     */
    @Column(name = "cdemo_menu_opt_name_4")
    @JsonProperty("CDEMO_MENU_OPT_NAME_4")
    private String cdemoMenuOptName4;

    /**
     * Original: CDEMO-MENU-OPT-NAME X(35) occurrence 5 (line 96)
     * PIC clause: X(35)
     */
    @Column(name = "cdemo_menu_opt_name_5")
    @JsonProperty("CDEMO_MENU_OPT_NAME_5")
    private String cdemoMenuOptName5;

    /**
     * Original: CDEMO-MENU-OPT-NAME X(35) occurrence 6 (line 96)
     * PIC clause: X(35)
     */
    @Column(name = "cdemo_menu_opt_name_6")
    @JsonProperty("CDEMO_MENU_OPT_NAME_6")
    private String cdemoMenuOptName6;

    /**
     * Original: CDEMO-MENU-OPT-NAME X(35) occurrence 7 (line 96)
     * PIC clause: X(35)
     */
    @Column(name = "cdemo_menu_opt_name_7")
    @JsonProperty("CDEMO_MENU_OPT_NAME_7")
    private String cdemoMenuOptName7;

    /**
     * Original: CDEMO-MENU-OPT-NAME X(35) occurrence 8 (line 96)
     * PIC clause: X(35)
     */
    @Column(name = "cdemo_menu_opt_name_8")
    @JsonProperty("CDEMO_MENU_OPT_NAME_8")
    private String cdemoMenuOptName8;

    /**
     * Original: CDEMO-MENU-OPT-NAME X(35) occurrence 9 (line 96)
     * PIC clause: X(35)
     */
    @Column(name = "cdemo_menu_opt_name_9")
    @JsonProperty("CDEMO_MENU_OPT_NAME_9")
    private String cdemoMenuOptName9;

    /**
     * Original: CDEMO-MENU-OPT-NAME X(35) occurrence 10 (line 96)
     * PIC clause: X(35)
     */
    @Column(name = "cdemo_menu_opt_name_10")
    @JsonProperty("CDEMO_MENU_OPT_NAME_10")
    private String cdemoMenuOptName10;

    /**
     * Original: CDEMO-MENU-OPT-NAME X(35) occurrence 11 (line 96)
     * PIC clause: X(35)
     */
    @Column(name = "cdemo_menu_opt_name_11")
    @JsonProperty("CDEMO_MENU_OPT_NAME_11")
    private String cdemoMenuOptName11;

    /**
     * Original: CDEMO-MENU-OPT-NAME X(35) occurrence 12 (line 96)
     * PIC clause: X(35)
     */
    @Column(name = "cdemo_menu_opt_name_12")
    @JsonProperty("CDEMO_MENU_OPT_NAME_12")
    private String cdemoMenuOptName12;

    /**
     * Original: CDEMO-MENU-OPT-PGMNAME X(08) (line 97)
     * PIC clause: X(08)
     */
    @Column(name = "cdemo_menu_opt_pgmname")
    @JsonProperty("CDEMO_MENU_OPT_PGMNAME")
    private String cdemoMenuOptPgmname;

    /**
     * Original: CDEMO-MENU-OPT-PGMNAME X(08) occurrence 2 (line 97)
     * PIC clause: X(08)
     */
    @Column(name = "cdemo_menu_opt_pgmname_2")
    @JsonProperty("CDEMO_MENU_OPT_PGMNAME_2")
    private String cdemoMenuOptPgmname2;

    /**
     * Original: CDEMO-MENU-OPT-PGMNAME X(08) occurrence 3 (line 97)
     * PIC clause: X(08)
     */
    @Column(name = "cdemo_menu_opt_pgmname_3")
    @JsonProperty("CDEMO_MENU_OPT_PGMNAME_3")
    private String cdemoMenuOptPgmname3;

    /**
     * Original: CDEMO-MENU-OPT-PGMNAME X(08) occurrence 4 (line 97)
     * PIC clause: X(08)
     */
    @Column(name = "cdemo_menu_opt_pgmname_4")
    @JsonProperty("CDEMO_MENU_OPT_PGMNAME_4")
    private String cdemoMenuOptPgmname4;

    /**
     * Original: CDEMO-MENU-OPT-PGMNAME X(08) occurrence 5 (line 97)
     * PIC clause: X(08)
     */
    @Column(name = "cdemo_menu_opt_pgmname_5")
    @JsonProperty("CDEMO_MENU_OPT_PGMNAME_5")
    private String cdemoMenuOptPgmname5;

    /**
     * Original: CDEMO-MENU-OPT-PGMNAME X(08) occurrence 6 (line 97)
     * PIC clause: X(08)
     */
    @Column(name = "cdemo_menu_opt_pgmname_6")
    @JsonProperty("CDEMO_MENU_OPT_PGMNAME_6")
    private String cdemoMenuOptPgmname6;

    /**
     * Original: CDEMO-MENU-OPT-PGMNAME X(08) occurrence 7 (line 97)
     * PIC clause: X(08)
     */
    @Column(name = "cdemo_menu_opt_pgmname_7")
    @JsonProperty("CDEMO_MENU_OPT_PGMNAME_7")
    private String cdemoMenuOptPgmname7;

    /**
     * Original: CDEMO-MENU-OPT-PGMNAME X(08) occurrence 8 (line 97)
     * PIC clause: X(08)
     */
    @Column(name = "cdemo_menu_opt_pgmname_8")
    @JsonProperty("CDEMO_MENU_OPT_PGMNAME_8")
    private String cdemoMenuOptPgmname8;

    /**
     * Original: CDEMO-MENU-OPT-PGMNAME X(08) occurrence 9 (line 97)
     * PIC clause: X(08)
     */
    @Column(name = "cdemo_menu_opt_pgmname_9")
    @JsonProperty("CDEMO_MENU_OPT_PGMNAME_9")
    private String cdemoMenuOptPgmname9;

    /**
     * Original: CDEMO-MENU-OPT-PGMNAME X(08) occurrence 10 (line 97)
     * PIC clause: X(08)
     */
    @Column(name = "cdemo_menu_opt_pgmname_10")
    @JsonProperty("CDEMO_MENU_OPT_PGMNAME_10")
    private String cdemoMenuOptPgmname10;

    /**
     * Original: CDEMO-MENU-OPT-PGMNAME X(08) occurrence 11 (line 97)
     * PIC clause: X(08)
     */
    @Column(name = "cdemo_menu_opt_pgmname_11")
    @JsonProperty("CDEMO_MENU_OPT_PGMNAME_11")
    private String cdemoMenuOptPgmname11;

    /**
     * Original: CDEMO-MENU-OPT-PGMNAME X(08) occurrence 12 (line 97)
     * PIC clause: X(08)
     */
    @Column(name = "cdemo_menu_opt_pgmname_12")
    @JsonProperty("CDEMO_MENU_OPT_PGMNAME_12")
    private String cdemoMenuOptPgmname12;

    /**
     * Original: CDEMO-MENU-OPT-USRTYPE X(01) (line 98)
     * PIC clause: X(01)
     */
    @Column(name = "cdemo_menu_opt_usrtype")
    @JsonProperty("CDEMO_MENU_OPT_USRTYPE")
    private String cdemoMenuOptUsrtype;

    /**
     * Original: CDEMO-MENU-OPT-USRTYPE X(01) occurrence 2 (line 98)
     * PIC clause: X(01)
     */
    @Column(name = "cdemo_menu_opt_usrtype_2")
    @JsonProperty("CDEMO_MENU_OPT_USRTYPE_2")
    private String cdemoMenuOptUsrtype2;

    /**
     * Original: CDEMO-MENU-OPT-USRTYPE X(01) occurrence 3 (line 98)
     * PIC clause: X(01)
     */
    @Column(name = "cdemo_menu_opt_usrtype_3")
    @JsonProperty("CDEMO_MENU_OPT_USRTYPE_3")
    private String cdemoMenuOptUsrtype3;

    /**
     * Original: CDEMO-MENU-OPT-USRTYPE X(01) occurrence 4 (line 98)
     * PIC clause: X(01)
     */
    @Column(name = "cdemo_menu_opt_usrtype_4")
    @JsonProperty("CDEMO_MENU_OPT_USRTYPE_4")
    private String cdemoMenuOptUsrtype4;

    /**
     * Original: CDEMO-MENU-OPT-USRTYPE X(01) occurrence 5 (line 98)
     * PIC clause: X(01)
     */
    @Column(name = "cdemo_menu_opt_usrtype_5")
    @JsonProperty("CDEMO_MENU_OPT_USRTYPE_5")
    private String cdemoMenuOptUsrtype5;

    /**
     * Original: CDEMO-MENU-OPT-USRTYPE X(01) occurrence 6 (line 98)
     * PIC clause: X(01)
     */
    @Column(name = "cdemo_menu_opt_usrtype_6")
    @JsonProperty("CDEMO_MENU_OPT_USRTYPE_6")
    private String cdemoMenuOptUsrtype6;

    /**
     * Original: CDEMO-MENU-OPT-USRTYPE X(01) occurrence 7 (line 98)
     * PIC clause: X(01)
     */
    @Column(name = "cdemo_menu_opt_usrtype_7")
    @JsonProperty("CDEMO_MENU_OPT_USRTYPE_7")
    private String cdemoMenuOptUsrtype7;

    /**
     * Original: CDEMO-MENU-OPT-USRTYPE X(01) occurrence 8 (line 98)
     * PIC clause: X(01)
     */
    @Column(name = "cdemo_menu_opt_usrtype_8")
    @JsonProperty("CDEMO_MENU_OPT_USRTYPE_8")
    private String cdemoMenuOptUsrtype8;

    /**
     * Original: CDEMO-MENU-OPT-USRTYPE X(01) occurrence 9 (line 98)
     * PIC clause: X(01)
     */
    @Column(name = "cdemo_menu_opt_usrtype_9")
    @JsonProperty("CDEMO_MENU_OPT_USRTYPE_9")
    private String cdemoMenuOptUsrtype9;

    /**
     * Original: CDEMO-MENU-OPT-USRTYPE X(01) occurrence 10 (line 98)
     * PIC clause: X(01)
     */
    @Column(name = "cdemo_menu_opt_usrtype_10")
    @JsonProperty("CDEMO_MENU_OPT_USRTYPE_10")
    private String cdemoMenuOptUsrtype10;

    /**
     * Original: CDEMO-MENU-OPT-USRTYPE X(01) occurrence 11 (line 98)
     * PIC clause: X(01)
     */
    @Column(name = "cdemo_menu_opt_usrtype_11")
    @JsonProperty("CDEMO_MENU_OPT_USRTYPE_11")
    private String cdemoMenuOptUsrtype11;

    /**
     * Original: CDEMO-MENU-OPT-USRTYPE X(01) occurrence 12 (line 98)
     * PIC clause: X(01)
     */
    @Column(name = "cdemo_menu_opt_usrtype_12")
    @JsonProperty("CDEMO_MENU_OPT_USRTYPE_12")
    private String cdemoMenuOptUsrtype12;

}
