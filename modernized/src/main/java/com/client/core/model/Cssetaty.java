package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: CSSETATY
 * @citation app/cpy/CSSETATY.cpy
 * 4 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "cssetaty")
public class Cssetaty implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * DB2 DCLGEN column OR FLG
     * PIC clause: X(50)
     */
    @Id
    @Column(name = "or", nullable = false)
    @JsonProperty("OR")
    private String or;

    /**
     * DB2 DCLGEN column AND CDEMO
     * PIC clause: X(50)
     */
    @Column(name = "and")
    @JsonProperty("AND")
    private String and;

    /**
     * DB2 DCLGEN column MOVE DFHRED
     * PIC clause: X(50)
     */
    @Column(name = "move")
    @JsonProperty("MOVE")
    private String move;

    /**
     * DB2 DCLGEN column IF FLG
     * PIC clause: X(50)
     */
    @Column(name = "if")
    @JsonProperty("IF")
    private String ifValue;

}
