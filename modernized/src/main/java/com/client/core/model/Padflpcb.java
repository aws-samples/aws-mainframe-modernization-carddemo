package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: PADFLPCB
 * @citation app/app-authorization-ims-db2-mq/cpy/PADFLPCB.CPY
 * 8 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "padflpcb")
public class Padflpcb implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: PADFL-DBDNAME X(08) (line 18)
     * PIC clause: X(08)
     */
    @Column(name = "padfl_dbdname")
    @JsonProperty("PADFL_DBDNAME")
    private String padflDbdname;

    /**
     * Original: PADFL-SEG-LEVEL X(02) (line 19)
     * PIC clause: X(02)
     */
    @Column(name = "padfl_seg_level")
    @JsonProperty("PADFL_SEG_LEVEL")
    private String padflSegLevel;

    /**
     * Original: PADFL-PCB-STATUS X(02) (line 20)
     * PIC clause: X(02)
     */
    @Column(name = "padfl_pcb_status")
    @JsonProperty("PADFL_PCB_STATUS")
    private String padflPcbStatus;

    /**
     * Original: PADFL-PCB-PROCOPT X(04) (line 21)
     * PIC clause: X(04)
     */
    @Column(name = "padfl_pcb_procopt")
    @JsonProperty("PADFL_PCB_PROCOPT")
    private String padflPcbProcopt;

    /**
     * Original: PADFL-SEG-NAME X(08) (line 23)
     * PIC clause: X(08)
     */
    @Column(name = "padfl_seg_name")
    @JsonProperty("PADFL_SEG_NAME")
    private String padflSegName;

    /**
     * Original: PADFL-KEYFB-NAME S9(05) (line 24)
     * PIC clause: S9(05)
     */
    @Id
    @Column(name = "padfl_keyfb_name", nullable = false)
    @JsonProperty("PADFL_KEYFB_NAME")
    private Integer padflKeyfbName;

    /**
     * Original: PADFL-NUM-SENSEGS S9(05) (line 25)
     * PIC clause: S9(05)
     */
    @Column(name = "padfl_num_sensegs")
    @JsonProperty("PADFL_NUM_SENSEGS")
    private Integer padflNumSensegs;

    /**
     * Original: PADFL-KEYFB X(255) (line 26)
     * PIC clause: X(255)
     */
    @Column(name = "padfl_keyfb")
    @JsonProperty("PADFL_KEYFB")
    private String padflKeyfb;

}
