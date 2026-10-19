package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: DCLTRCAT
 * @citation app/app-transaction-type-db2/dcl/DCLTRCAT.dcl
 * 4 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "dcltrcat")
public class Dcltrcat implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: DCL-TRC-TYPE-CODE X(2) (line 39)
     * PIC clause: X(2)
     */
    @Id
    @Column(name = "dcl_trc_type_code", nullable = false)
    @JsonProperty("DCL_TRC_TYPE_CODE")
    private String dclTrcTypeCode;

    /**
     * Original: DCL-TRC-TYPE-CATEGORY X(4) (line 42)
     * PIC clause: X(4)
     */
    @Column(name = "dcl_trc_type_category")
    @JsonProperty("DCL_TRC_TYPE_CATEGORY")
    private String dclTrcTypeCategory;

    /**
     * Original: DCL-TRC-CAT-DATA-LEN S9(4) (line 47)
     * PIC clause: S9(4)
     */
    @Column(name = "dcl_trc_cat_data_len")
    @JsonProperty("DCL_TRC_CAT_DATA_LEN")
    private Integer dclTrcCatDataLen;

    /**
     * Original: DCL-TRC-CAT-DATA-TEXT X(50) (line 50)
     * PIC clause: X(50)
     */
    @Column(name = "dcl_trc_cat_data_text")
    @JsonProperty("DCL_TRC_CAT_DATA_TEXT")
    private String dclTrcCatDataText;

}
