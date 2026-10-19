package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: CSMSG02Y
 * @citation app/cpy/CSMSG02Y.cpy
 * 4 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "csmsg02y")
public class Csmsg02y implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: ABEND-CODE X(4) (line 22)
     * PIC clause: X(4)
     */
    @Id
    @Column(name = "abend_code", nullable = false)
    @JsonProperty("ABEND_CODE")
    private String abendCode;

    /**
     * Original: ABEND-CULPRIT X(8) (line 24)
     * PIC clause: X(8)
     */
    @Column(name = "abend_culprit")
    @JsonProperty("ABEND_CULPRIT")
    private String abendCulprit;

    /**
     * Original: ABEND-REASON X(50) (line 26)
     * PIC clause: X(50)
     */
    @Column(name = "abend_reason")
    @JsonProperty("ABEND_REASON")
    private String abendReason;

    /**
     * Original: ABEND-MSG X(72) (line 28)
     * PIC clause: X(72)
     */
    @Column(name = "abend_msg")
    @JsonProperty("ABEND_MSG")
    private String abendMsg;

}
