package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: TRNTYPE
 * @citation app/app-transaction-type-db2/ddl/TRNTYPE.ddl
 * 1 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "trntype")
public class Trntype implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * DB2 DCLGEN column TR_DESCRIPTION VARCHAR(50)
     * PIC clause: X(50)
     */
    @Id
    @Column(name = "tr_description", nullable = false)
    @JsonProperty("TR_DESCRIPTION")
    private String trDescription;

}
