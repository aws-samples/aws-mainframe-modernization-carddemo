package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: PASFLPCB
 * @citation app/app-authorization-ims-db2-mq/cpy/PASFLPCB.CPY
 * 8 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "pasflpcb")
public class Pasflpcb implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: PASFL-DBDNAME X(08) (line 18)
     * PIC clause: X(08)
     */
    @Column(name = "pasfl_dbdname")
    @JsonProperty("PASFL_DBDNAME")
    private String pasflDbdname;

    /**
     * Original: PASFL-SEG-LEVEL X(02) (line 19)
     * PIC clause: X(02)
     */
    @Column(name = "pasfl_seg_level")
    @JsonProperty("PASFL_SEG_LEVEL")
    private String pasflSegLevel;

    /**
     * Original: PASFL-PCB-STATUS X(02) (line 20)
     * PIC clause: X(02)
     */
    @Column(name = "pasfl_pcb_status")
    @JsonProperty("PASFL_PCB_STATUS")
    private String pasflPcbStatus;

    /**
     * Original: PASFL-PCB-PROCOPT X(04) (line 21)
     * PIC clause: X(04)
     */
    @Column(name = "pasfl_pcb_procopt")
    @JsonProperty("PASFL_PCB_PROCOPT")
    private String pasflPcbProcopt;

    /**
     * Original: PASFL-SEG-NAME X(08) (line 23)
     * PIC clause: X(08)
     */
    @Column(name = "pasfl_seg_name")
    @JsonProperty("PASFL_SEG_NAME")
    private String pasflSegName;

    /**
     * Original: PASFL-KEYFB-NAME S9(05) (line 24)
     * PIC clause: S9(05)
     */
    @Id
    @Column(name = "pasfl_keyfb_name", nullable = false)
    @JsonProperty("PASFL_KEYFB_NAME")
    private Integer pasflKeyfbName;

    /**
     * Original: PASFL-NUM-SENSEGS S9(05) (line 25)
     * PIC clause: S9(05)
     */
    @Column(name = "pasfl_num_sensegs")
    @JsonProperty("PASFL_NUM_SENSEGS")
    private Integer pasflNumSensegs;

    /**
     * Original: PASFL-KEYFB X(100) (line 26)
     * PIC clause: X(100)
     */
    @Column(name = "pasfl_keyfb")
    @JsonProperty("PASFL_KEYFB")
    private String pasflKeyfb;

}
