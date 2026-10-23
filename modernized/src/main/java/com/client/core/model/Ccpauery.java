package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: CCPAUERY
 * @citation app/app-authorization-ims-db2-mq/cpy/CCPAUERY.cpy
 * 11 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "ccpauery")
public class Ccpauery implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: ERR-DATE X(06) (line 20)
     * PIC clause: X(06)
     */
    @Column(name = "err_date")
    @JsonProperty("ERR_DATE")
    private String errDate;

    /**
     * Original: ERR-TIME X(06) (line 21)
     * PIC clause: X(06)
     */
    @Column(name = "err_time")
    @JsonProperty("ERR_TIME")
    private String errTime;

    /**
     * Original: ERR-APPLICATION X(08) (line 22)
     * PIC clause: X(08)
     */
    @Column(name = "err_application")
    @JsonProperty("ERR_APPLICATION")
    private String errApplication;

    /**
     * Original: ERR-PROGRAM X(08) (line 23)
     * PIC clause: X(08)
     */
    @Column(name = "err_program")
    @JsonProperty("ERR_PROGRAM")
    private String errProgram;

    /**
     * Original: ERR-LOCATION X(04) (line 24)
     * PIC clause: X(04)
     */
    @Column(name = "err_location")
    @JsonProperty("ERR_LOCATION")
    private String errLocation;

    /**
     * Original: ERR-LEVEL X(01) (line 25)
     * PIC clause: X(01)
     */
    @Column(name = "err_level")
    @JsonProperty("ERR_LEVEL")
    private String errLevel;

    /**
     * Original: ERR-SUBSYSTEM X(01) (line 30)
     * PIC clause: X(01)
     */
    @Column(name = "err_subsystem")
    @JsonProperty("ERR_SUBSYSTEM")
    private String errSubsystem;

    /**
     * Original: ERR-CODE-1 X(09) (line 37)
     * PIC clause: X(09)
     */
    @Id
    @Column(name = "err_code_1", nullable = false)
    @JsonProperty("ERR_CODE_1")
    private String errCode1;

    /**
     * Original: ERR-CODE-2 X(09) (line 38)
     * PIC clause: X(09)
     */
    @Column(name = "err_code_2")
    @JsonProperty("ERR_CODE_2")
    private String errCode2;

    /**
     * Original: ERR-MESSAGE X(50) (line 39)
     * PIC clause: X(50)
     */
    @Column(name = "err_message")
    @JsonProperty("ERR_MESSAGE")
    private String errMessage;

    /**
     * Original: ERR-EVENT-KEY X(20) (line 40)
     * PIC clause: X(20)
     */
    @Column(name = "err_event_key")
    @JsonProperty("ERR_EVENT_KEY")
    private String errEventKey;

}
