package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: CSLKPCDY
 * @citation app/cpy/CSLKPCDY.cpy
 * 4 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "cslkpcdy")
public class Cslkpcdy implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: WS-US-PHONE-AREA-CODE-TO-EDIT XXX (line 24)
     * PIC clause: XXX
     */
    @Id
    @Column(name = "ws_us_phone_area_code_to_edit", nullable = false)
    @JsonProperty("WS_US_PHONE_AREA_CODE_TO_EDIT")
    private String wsUsPhoneAreaCodeToEdit;

    /**
     * Original: US-STATE-CODE-TO-EDIT X(2) (line 1012)
     * PIC clause: X(2)
     */
    @Column(name = "us_state_code_to_edit")
    @JsonProperty("US_STATE_CODE_TO_EDIT")
    private String usStateCodeToEdit;

    /**
     * Original: US-STATE-AND-FIRST-ZIP2 X(4) (line 1072)
     * PIC clause: X(4)
     */
    @Column(name = "us_state_and_first_zip2")
    @JsonProperty("US_STATE_AND_FIRST_ZIP2")
    private String usStateAndFirstZip2;

    /**
     * Original: LAST-3-OF-ZIP X(3) (line 1314)
     * PIC clause: X(3)
     */
    @Column(name = "last_3_of_zip")
    @JsonProperty("LAST_3_OF_ZIP")
    private String last3OfZip;

}
