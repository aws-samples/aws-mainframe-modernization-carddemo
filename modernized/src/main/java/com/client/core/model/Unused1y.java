package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: UNUSED1Y
 * @citation app/cpy/UNUSED1Y.cpy
 * 6 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "unused1y")
public class Unused1y implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: UNUSED-ID X(08) (line 2)
     * PIC clause: X(08)
     */
    @Id
    @Column(name = "unused_id", nullable = false)
    @JsonProperty("UNUSED_ID")
    private String unusedId;

    /**
     * Original: UNUSED-FNAME X(20) (line 3)
     * PIC clause: X(20)
     */
    @Column(name = "unused_fname")
    @JsonProperty("UNUSED_FNAME")
    private String unusedFname;

    /**
     * Original: UNUSED-LNAME X(20) (line 4)
     * PIC clause: X(20)
     */
    @Column(name = "unused_lname")
    @JsonProperty("UNUSED_LNAME")
    private String unusedLname;

    /**
     * Original: UNUSED-PWD X(08) (line 5)
     * PIC clause: X(08)
     */
    @Column(name = "unused_pwd")
    @JsonProperty("UNUSED_PWD")
    private String unusedPwd;

    /**
     * Original: UNUSED-TYPE X(01) (line 6)
     * PIC clause: X(01)
     */
    @Column(name = "unused_type")
    @JsonProperty("UNUSED_TYPE")
    private String unusedType;

    /**
     * Original: UNUSED-FILLER X(23) (line 7)
     * PIC clause: X(23)
     */
    @Column(name = "unused_filler")
    @JsonProperty("UNUSED_FILLER")
    private String unusedFiller;

}
