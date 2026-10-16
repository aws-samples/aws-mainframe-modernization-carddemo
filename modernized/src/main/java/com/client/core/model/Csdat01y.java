package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: CSDAT01Y
 * @citation app/cpy/CSDAT01Y.cpy
 * 22 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "csdat01y")
public class Csdat01y implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: WS-CURDATE-YEAR 9(04) (line 20)
     * PIC clause: 9(04)
     */
    @Id
    @Column(name = "ws_curdate_year", nullable = false)
    @JsonProperty("WS_CURDATE_YEAR")
    private Integer wsCurdateYear;

    /**
     * Original: WS-CURDATE-MONTH 9(02) (line 21)
     * PIC clause: 9(02)
     */
    @Column(name = "ws_curdate_month")
    @JsonProperty("WS_CURDATE_MONTH")
    private Integer wsCurdateMonth;

    /**
     * Original: WS-CURDATE-DAY 9(02) (line 22)
     * PIC clause: 9(02)
     */
    @Column(name = "ws_curdate_day")
    @JsonProperty("WS_CURDATE_DAY")
    private Integer wsCurdateDay;

    /**
     * Original: WS-CURDATE-N 9(08) (line 23)
     * PIC clause: 9(08)
     */
    @Column(name = "ws_curdate_n")
    @JsonProperty("WS_CURDATE_N")
    private Integer wsCurdateN;

    /**
     * Original: WS-CURTIME-HOURS 9(02) (line 25)
     * PIC clause: 9(02)
     */
    @Column(name = "ws_curtime_hours")
    @JsonProperty("WS_CURTIME_HOURS")
    private Integer wsCurtimeHours;

    /**
     * Original: WS-CURTIME-MINUTE 9(02) (line 26)
     * PIC clause: 9(02)
     */
    @Column(name = "ws_curtime_minute")
    @JsonProperty("WS_CURTIME_MINUTE")
    private Integer wsCurtimeMinute;

    /**
     * Original: WS-CURTIME-SECOND 9(02) (line 27)
     * PIC clause: 9(02)
     */
    @Column(name = "ws_curtime_second")
    @JsonProperty("WS_CURTIME_SECOND")
    private Integer wsCurtimeSecond;

    /**
     * Original: WS-CURTIME-MILSEC 9(02) (line 28)
     * PIC clause: 9(02)
     */
    @Column(name = "ws_curtime_milsec")
    @JsonProperty("WS_CURTIME_MILSEC")
    private Integer wsCurtimeMilsec;

    /**
     * Original: WS-CURTIME-N 9(08) (line 29)
     * PIC clause: 9(08)
     */
    @Column(name = "ws_curtime_n")
    @JsonProperty("WS_CURTIME_N")
    private Integer wsCurtimeN;

    /**
     * Original: WS-CURDATE-MM 9(02) (line 31)
     * PIC clause: 9(02)
     */
    @Column(name = "ws_curdate_mm")
    @JsonProperty("WS_CURDATE_MM")
    private Integer wsCurdateMm;

    /**
     * Original: WS-CURDATE-DD 9(02) (line 33)
     * PIC clause: 9(02)
     */
    @Column(name = "ws_curdate_dd")
    @JsonProperty("WS_CURDATE_DD")
    private Integer wsCurdateDd;

    /**
     * Original: WS-CURDATE-YY 9(02) (line 35)
     * PIC clause: 9(02)
     */
    @Column(name = "ws_curdate_yy")
    @JsonProperty("WS_CURDATE_YY")
    private Integer wsCurdateYy;

    /**
     * Original: WS-CURTIME-HH 9(02) (line 37)
     * PIC clause: 9(02)
     */
    @Column(name = "ws_curtime_hh")
    @JsonProperty("WS_CURTIME_HH")
    private Integer wsCurtimeHh;

    /**
     * Original: WS-CURTIME-MM 9(02) (line 39)
     * PIC clause: 9(02)
     */
    @Column(name = "ws_curtime_mm")
    @JsonProperty("WS_CURTIME_MM")
    private Integer wsCurtimeMm;

    /**
     * Original: WS-CURTIME-SS 9(02) (line 41)
     * PIC clause: 9(02)
     */
    @Column(name = "ws_curtime_ss")
    @JsonProperty("WS_CURTIME_SS")
    private Integer wsCurtimeSs;

    /**
     * Original: WS-TIMESTAMP-DT-YYYY 9(04) (line 43)
     * PIC clause: 9(04)
     */
    @Column(name = "ws_timestamp_dt_yyyy")
    @JsonProperty("WS_TIMESTAMP_DT_YYYY")
    private Integer wsTimestampDtYyyy;

    /**
     * Original: WS-TIMESTAMP-DT-MM 9(02) (line 45)
     * PIC clause: 9(02)
     */
    @Column(name = "ws_timestamp_dt_mm")
    @JsonProperty("WS_TIMESTAMP_DT_MM")
    private Integer wsTimestampDtMm;

    /**
     * Original: WS-TIMESTAMP-DT-DD 9(02) (line 47)
     * PIC clause: 9(02)
     */
    @Column(name = "ws_timestamp_dt_dd")
    @JsonProperty("WS_TIMESTAMP_DT_DD")
    private Integer wsTimestampDtDd;

    /**
     * Original: WS-TIMESTAMP-TM-HH 9(02) (line 49)
     * PIC clause: 9(02)
     */
    @Column(name = "ws_timestamp_tm_hh")
    @JsonProperty("WS_TIMESTAMP_TM_HH")
    private Integer wsTimestampTmHh;

    /**
     * Original: WS-TIMESTAMP-TM-MM 9(02) (line 51)
     * PIC clause: 9(02)
     */
    @Column(name = "ws_timestamp_tm_mm")
    @JsonProperty("WS_TIMESTAMP_TM_MM")
    private Integer wsTimestampTmMm;

    /**
     * Original: WS-TIMESTAMP-TM-SS 9(02) (line 53)
     * PIC clause: 9(02)
     */
    @Column(name = "ws_timestamp_tm_ss")
    @JsonProperty("WS_TIMESTAMP_TM_SS")
    private Integer wsTimestampTmSs;

    /**
     * Original: WS-TIMESTAMP-TM-MS6 9(06) (line 55)
     * PIC clause: 9(06)
     */
    @Column(name = "ws_timestamp_tm_ms6")
    @JsonProperty("WS_TIMESTAMP_TM_MS6")
    private Integer wsTimestampTmMs6;

}
