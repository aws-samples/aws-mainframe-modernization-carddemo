package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: COTTL01Y
 * @citation app/cpy/COTTL01Y.cpy
 * 3 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "cottl01y")
public class Cottl01y implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: CCDA-TITLE01 X(40) (line 18)
     * PIC clause: X(40)
     */
    @Id
    @Column(name = "ccda_title01", nullable = false)
    @JsonProperty("CCDA_TITLE01")
    private String ccdaTitle01;

    /**
     * Original: CCDA-TITLE02 X(40) (line 20)
     * PIC clause: X(40)
     */
    @Column(name = "ccda_title02")
    @JsonProperty("CCDA_TITLE02")
    private String ccdaTitle02;

    /**
     * Original: CCDA-THANK-YOU X(40) (line 23)
     * PIC clause: X(40)
     */
    @Column(name = "ccda_thank_you")
    @JsonProperty("CCDA_THANK_YOU")
    private String ccdaThankYou;

}
