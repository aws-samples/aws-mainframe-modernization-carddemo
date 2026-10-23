package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: CSDB2RWY
 * @citation app/app-transaction-type-db2/cpy/CSDB2RWY.cpy
 * 19 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "csdb2rwy")
public class Csdb2rwy implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: WS-DISP-SQLCODE ----9 (line 22)
     * PIC clause: ----9
     */
    @Id
    @Column(name = "ws_disp_sqlcode", nullable = false)
    @JsonProperty("WS_DISP_SQLCODE")
    private String wsDispSqlcode;

    /**
     * Original: WS-DUMMY-DB2-INT S9(4) (line 23)
     * PIC clause: S9(4)
     */
    @Column(name = "ws_dummy_db2_int")
    @JsonProperty("WS_DUMMY_DB2_INT")
    private Integer wsDummyDb2Int;

    /**
     * Original: WS-DB2-PROCESSING-FLAG X(1) (line 25)
     * PIC clause: X(1)
     */
    @Column(name = "ws_db2_processing_flag")
    @JsonProperty("WS_DB2_PROCESSING_FLAG")
    private String wsDb2ProcessingFlag;

    /**
     * Original: WS-DB2-CURRENT-ACTION X(72) (line 28)
     * PIC clause: X(72)
     */
    @Column(name = "ws_db2_current_action")
    @JsonProperty("WS_DB2_CURRENT_ACTION")
    private String wsDb2CurrentAction;

    /**
     * Original: WS-DSNTIAC-MESG-LEN S9(4) (line 34)
     * PIC clause: S9(4)
     */
    @Column(name = "ws_dsntiac_mesg_len")
    @JsonProperty("WS_DSNTIAC_MESG_LEN")
    private Integer wsDsntiacMesgLen;

    /**
     * Original: WS-DSNTIAC-FMTD-TEXT-LINE X(72) (line 36)
     * PIC clause: X(72)
     */
    @Column(name = "ws_dsntiac_fmtd_text_line")
    @JsonProperty("WS_DSNTIAC_FMTD_TEXT_LINE")
    private String wsDsntiacFmtdTextLine;

    /**
     * Original: WS-DSNTIAC-FMTD-TEXT-LINE X(72) occurrence 2 (line 36)
     * PIC clause: X(72)
     */
    @Column(name = "ws_dsntiac_fmtd_text_line_2")
    @JsonProperty("WS_DSNTIAC_FMTD_TEXT_LINE_2")
    private String wsDsntiacFmtdTextLine2;

    /**
     * Original: WS-DSNTIAC-FMTD-TEXT-LINE X(72) occurrence 3 (line 36)
     * PIC clause: X(72)
     */
    @Column(name = "ws_dsntiac_fmtd_text_line_3")
    @JsonProperty("WS_DSNTIAC_FMTD_TEXT_LINE_3")
    private String wsDsntiacFmtdTextLine3;

    /**
     * Original: WS-DSNTIAC-FMTD-TEXT-LINE X(72) occurrence 4 (line 36)
     * PIC clause: X(72)
     */
    @Column(name = "ws_dsntiac_fmtd_text_line_4")
    @JsonProperty("WS_DSNTIAC_FMTD_TEXT_LINE_4")
    private String wsDsntiacFmtdTextLine4;

    /**
     * Original: WS-DSNTIAC-FMTD-TEXT-LINE X(72) occurrence 5 (line 36)
     * PIC clause: X(72)
     */
    @Column(name = "ws_dsntiac_fmtd_text_line_5")
    @JsonProperty("WS_DSNTIAC_FMTD_TEXT_LINE_5")
    private String wsDsntiacFmtdTextLine5;

    /**
     * Original: WS-DSNTIAC-FMTD-TEXT-LINE X(72) occurrence 6 (line 36)
     * PIC clause: X(72)
     */
    @Column(name = "ws_dsntiac_fmtd_text_line_6")
    @JsonProperty("WS_DSNTIAC_FMTD_TEXT_LINE_6")
    private String wsDsntiacFmtdTextLine6;

    /**
     * Original: WS-DSNTIAC-FMTD-TEXT-LINE X(72) occurrence 7 (line 36)
     * PIC clause: X(72)
     */
    @Column(name = "ws_dsntiac_fmtd_text_line_7")
    @JsonProperty("WS_DSNTIAC_FMTD_TEXT_LINE_7")
    private String wsDsntiacFmtdTextLine7;

    /**
     * Original: WS-DSNTIAC-FMTD-TEXT-LINE X(72) occurrence 8 (line 36)
     * PIC clause: X(72)
     */
    @Column(name = "ws_dsntiac_fmtd_text_line_8")
    @JsonProperty("WS_DSNTIAC_FMTD_TEXT_LINE_8")
    private String wsDsntiacFmtdTextLine8;

    /**
     * Original: WS-DSNTIAC-FMTD-TEXT-LINE X(72) occurrence 9 (line 36)
     * PIC clause: X(72)
     */
    @Column(name = "ws_dsntiac_fmtd_text_line_9")
    @JsonProperty("WS_DSNTIAC_FMTD_TEXT_LINE_9")
    private String wsDsntiacFmtdTextLine9;

    /**
     * Original: WS-DSNTIAC-FMTD-TEXT-LINE X(72) occurrence 10 (line 36)
     * PIC clause: X(72)
     */
    @Column(name = "ws_dsntiac_fmtd_text_line_10")
    @JsonProperty("WS_DSNTIAC_FMTD_TEXT_LINE_10")
    private String wsDsntiacFmtdTextLine10;

    /**
     * Original: WS-DSNTIAC-LRECL S9(4) (line 41)
     * PIC clause: S9(4)
     */
    @Column(name = "ws_dsntiac_lrecl")
    @JsonProperty("WS_DSNTIAC_LRECL")
    private Integer wsDsntiacLrecl;

    /**
     * Original: WS-DSNTIAC-ERR-MSG X(10) (line 43)
     * PIC clause: X(10)
     */
    @Column(name = "ws_dsntiac_err_msg")
    @JsonProperty("WS_DSNTIAC_ERR_MSG")
    private String wsDsntiacErrMsg;

    /**
     * Original: WS-DSNTIAC-ERR-CD-X X(02) (line 44)
     * PIC clause: X(02)
     */
    @Column(name = "ws_dsntiac_err_cd_x")
    @JsonProperty("WS_DSNTIAC_ERR_CD_X")
    private String wsDsntiacErrCdX;

    /**
     * Original: WS-DSNTIAC-ERR-CD 9(02) (line 45)
     * PIC clause: 9(02)
     */
    @Column(name = "ws_dsntiac_err_cd")
    @JsonProperty("WS_DSNTIAC_ERR_CD")
    private Integer wsDsntiacErrCd;

}
