package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: XTRNTYCAT
 * @citation app/app-transaction-type-db2/ddl/XTRNTYCAT.ddl
 * 3 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "xtrntycat")
public class Xtrntycat implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * DB2 DCLGEN column ON CARDDEMO
     * PIC clause: X(50)
     */
    @Id
    @Column(name = "on", nullable = false)
    @JsonProperty("ON")
    private String on;

    /**
     * DB2 DCLGEN column ERASE NO
     * PIC clause: X(50)
     */
    @Column(name = "erase")
    @JsonProperty("ERASE")
    private String erase;

    /**
     * DB2 DCLGEN column CLOSE NO
     * PIC clause: X(50)
     */
    @Column(name = "close")
    @JsonProperty("CLOSE")
    private String close;

}
