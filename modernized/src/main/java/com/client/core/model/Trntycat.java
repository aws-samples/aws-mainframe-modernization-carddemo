package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: TRNTYCAT
 * @citation app/app-transaction-type-db2/ddl/TRNTYCAT.ddl
 * 3 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "trntycat")
public class Trntycat implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * DB2 DCLGEN column TRC_TYPE_CATEGORY CHAR(4)
     * PIC clause: X(4)
     */
    @Id
    @Column(name = "trc_type_category", nullable = false)
    @JsonProperty("TRC_TYPE_CATEGORY")
    private String trcTypeCategory;

    /**
     * DB2 DCLGEN column TRC_CAT_DATA VARCHAR(50)
     * PIC clause: X(50)
     */
    @Column(name = "trc_cat_data")
    @JsonProperty("TRC_CAT_DATA")
    private String trcCatData;

    /**
     * DB2 DCLGEN column REFERENCES CARDDEMO
     * PIC clause: X(50)
     */
    @Column(name = "references")
    @JsonProperty("REFERENCES")
    private String references;

}
