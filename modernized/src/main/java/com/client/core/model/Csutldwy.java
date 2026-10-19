package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: CSUTLDWY
 * @citation app/cpy/CSUTLDWY.cpy
 * 25 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "csutldwy")
public class Csutldwy implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: WS-EDIT-DATE-CC X(2) (line 6)
     * PIC clause: X(2)
     */
    @Id
    @Column(name = "ws_edit_date_cc", nullable = false)
    @JsonProperty("WS_EDIT_DATE_CC")
    private String wsEditDateCc;

    /**
     * Original: WS-EDIT-DATE-CC-N 9(2) (line 7)
     * PIC clause: 9(2)
     */
    @Column(name = "ws_edit_date_cc_n")
    @JsonProperty("WS_EDIT_DATE_CC_N")
    private Integer wsEditDateCcN;

    /**
     * Original: WS-EDIT-DATE-YY X(2) (line 11)
     * PIC clause: X(2)
     */
    @Column(name = "ws_edit_date_yy")
    @JsonProperty("WS_EDIT_DATE_YY")
    private String wsEditDateYy;

    /**
     * Original: WS-EDIT-DATE-YY-N 9(2) (line 12)
     * PIC clause: 9(2)
     */
    @Column(name = "ws_edit_date_yy_n")
    @JsonProperty("WS_EDIT_DATE_YY_N")
    private Integer wsEditDateYyN;

    /**
     * Original: WS-EDIT-DATE-CCYY-N 9(4) (line 14)
     * PIC clause: 9(4)
     */
    @Column(name = "ws_edit_date_ccyy_n")
    @JsonProperty("WS_EDIT_DATE_CCYY_N")
    private Integer wsEditDateCcyyN;

    /**
     * Original: WS-EDIT-DATE-MM X(2) (line 16)
     * PIC clause: X(2)
     */
    @Column(name = "ws_edit_date_mm")
    @JsonProperty("WS_EDIT_DATE_MM")
    private String wsEditDateMm;

    /**
     * Original: WS-EDIT-DATE-MM-N 9(2) (line 17)
     * PIC clause: 9(2)
     */
    @Column(name = "ws_edit_date_mm_n")
    @JsonProperty("WS_EDIT_DATE_MM_N")
    private Integer wsEditDateMmN;

    /**
     * Original: WS-EDIT-DATE-DD X(2) (line 25)
     * PIC clause: X(2)
     */
    @Column(name = "ws_edit_date_dd")
    @JsonProperty("WS_EDIT_DATE_DD")
    private String wsEditDateDd;

    /**
     * Original: WS-EDIT-DATE-DD-N 9(2) (line 26)
     * PIC clause: 9(2)
     */
    @Column(name = "ws_edit_date_dd_n")
    @JsonProperty("WS_EDIT_DATE_DD_N")
    private Integer wsEditDateDdN;

    /**
     * Original: WS-EDIT-DATE-CCYYMMDD-N 9(8) (line 35)
     * PIC clause: 9(8)
     */
    @Column(name = "ws_edit_date_ccyymmdd_n")
    @JsonProperty("WS_EDIT_DATE_CCYYMMDD_N")
    private Integer wsEditDateCcyymmddN;

    /**
     * Original: WS-EDIT-DATE-BINARY S9(9) (line 37)
     * PIC clause: S9(9)
     */
    @Column(name = "ws_edit_date_binary")
    @JsonProperty("WS_EDIT_DATE_BINARY")
    private Integer wsEditDateBinary;

    /**
     * Original: WS-CURRENT-DATE-YYYYMMDD X(8) (line 39)
     * PIC clause: X(8)
     */
    @Column(name = "ws_current_date_yyyymmdd")
    @JsonProperty("WS_CURRENT_DATE_YYYYMMDD")
    private String wsCurrentDateYyyymmdd;

    /**
     * Original: WS-CURRENT-DATE-YYYYMMDD-N 9(8) (line 40)
     * PIC clause: 9(8)
     */
    @Column(name = "ws_current_date_yyyymmdd_n")
    @JsonProperty("WS_CURRENT_DATE_YYYYMMDD_N")
    private Integer wsCurrentDateYyyymmddN;

    /**
     * Original: WS-CURRENT-DATE-BINARY S9(9) (line 42)
     * PIC clause: S9(9)
     */
    @Column(name = "ws_current_date_binary")
    @JsonProperty("WS_CURRENT_DATE_BINARY")
    private Integer wsCurrentDateBinary;

    /**
     * Original: WS-EDIT-YEAR-FLG X(01) (line 46)
     * PIC clause: X(01)
     */
    @Column(name = "ws_edit_year_flg")
    @JsonProperty("WS_EDIT_YEAR_FLG")
    private String wsEditYearFlg;

    /**
     * Original: WS-EDIT-MONTH X(01) (line 50)
     * PIC clause: X(01)
     */
    @Column(name = "ws_edit_month")
    @JsonProperty("WS_EDIT_MONTH")
    private String wsEditMonth;

    /**
     * Original: WS-EDIT-DAY X(01) (line 54)
     * PIC clause: X(01)
     */
    @Column(name = "ws_edit_day")
    @JsonProperty("WS_EDIT_DAY")
    private String wsEditDay;

    /**
     * Original: WS-DATE-FORMAT X(08) (line 58)
     * PIC clause: X(08)
     */
    @Column(name = "ws_date_format")
    @JsonProperty("WS_DATE_FORMAT")
    private String wsDateFormat;

    /**
     * Original: WS-SEVERITY X(04) (line 61)
     * PIC clause: X(04)
     */
    @Column(name = "ws_severity")
    @JsonProperty("WS_SEVERITY")
    private String wsSeverity;

    /**
     * Original: WS-SEVERITY-N 9(4) (line 62)
     * PIC clause: 9(4)
     */
    @Column(name = "ws_severity_n")
    @JsonProperty("WS_SEVERITY_N")
    private Integer wsSeverityN;

    /**
     * Original: WS-MSG-NO X(04) (line 66)
     * PIC clause: X(04)
     */
    @Column(name = "ws_msg_no")
    @JsonProperty("WS_MSG_NO")
    private String wsMsgNo;

    /**
     * Original: WS-MSG-NO-N 9(4) (line 67)
     * PIC clause: 9(4)
     */
    @Column(name = "ws_msg_no_n")
    @JsonProperty("WS_MSG_NO_N")
    private Integer wsMsgNoN;

    /**
     * Original: WS-RESULT X(15) (line 71)
     * PIC clause: X(15)
     */
    @Column(name = "ws_result")
    @JsonProperty("WS_RESULT")
    private String wsResult;

    /**
     * Original: WS-DATE X(10) (line 76)
     * PIC clause: X(10)
     */
    @Column(name = "ws_date")
    @JsonProperty("WS_DATE")
    private String wsDate;

    /**
     * Original: WS-DATE-FMT X(10) (line 81)
     * PIC clause: X(10)
     */
    @Column(name = "ws_date_fmt")
    @JsonProperty("WS_DATE_FMT")
    private String wsDateFmt;

}
