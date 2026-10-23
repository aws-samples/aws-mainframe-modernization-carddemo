package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: DCLTRTYP
 * @citation app/app-transaction-type-db2/dcl/DCLTRTYP.dcl
 * 3 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "dcltrtyp")
public class Dcltrtyp implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: DCL-TR-TYPE X(2) (line 38)
     * PIC clause: X(2)
     */
    @Id
    @Column(name = "dcl_tr_type", nullable = false)
    @JsonProperty("DCL_TR_TYPE")
    private String dclTrType;

    /**
     * Original: DCL-TR-DESCRIPTION-LEN S9(4) (line 42)
     * PIC clause: S9(4)
     */
    @Column(name = "dcl_tr_description_len")
    @JsonProperty("DCL_TR_DESCRIPTION_LEN")
    private Integer dclTrDescriptionLen;

    /**
     * Original: DCL-TR-DESCRIPTION-TEXT X(50) (line 45)
     * PIC clause: X(50)
     */
    @Column(name = "dcl_tr_description_text")
    @JsonProperty("DCL_TR_DESCRIPTION_TEXT")
    private String dclTrDescriptionText;

}
