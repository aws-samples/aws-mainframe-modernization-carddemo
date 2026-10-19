package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: XAUTHFRD
 * @citation app/app-authorization-ims-db2-mq/ddl/XAUTHFRD.ddl
 * 2 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "xauthfrd")
public class Xauthfrd implements Serializable {

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
     * DB2 DCLGEN column COPY YES
     * PIC clause: X(50)
     */
    @Column(name = "copy")
    @JsonProperty("COPY")
    private String copy;

}
