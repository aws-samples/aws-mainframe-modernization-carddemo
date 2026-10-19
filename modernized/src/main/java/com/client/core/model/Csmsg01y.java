package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: CSMSG01Y
 * @citation app/cpy/CSMSG01Y.cpy
 * 2 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "csmsg01y")
public class Csmsg01y implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: CCDA-MSG-THANK-YOU X(50) (line 18)
     * PIC clause: X(50)
     */
    @Column(name = "ccda_msg_thank_you")
    @JsonProperty("CCDA_MSG_THANK_YOU")
    private String ccdaMsgThankYou;

    /**
     * Original: CCDA-MSG-INVALID-KEY X(50) (line 20)
     * PIC clause: X(50)
     */
    @Id
    @Column(name = "ccda_msg_invalid_key", nullable = false)
    @JsonProperty("CCDA_MSG_INVALID_KEY")
    private String ccdaMsgInvalidKey;

}
