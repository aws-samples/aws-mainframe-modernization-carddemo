package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: IMSFUNCS
 * @citation app/app-authorization-ims-db2-mq/cpy/IMSFUNCS.cpy
 * 10 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "imsfuncs")
public class Imsfuncs implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: FUNC-GU X(04) (line 18)
     * PIC clause: X(04)
     */
    @Id
    @Column(name = "func_gu", nullable = false)
    @JsonProperty("FUNC_GU")
    private String funcGu;

    /**
     * Original: FUNC-GHU X(04) (line 19)
     * PIC clause: X(04)
     */
    @Column(name = "func_ghu")
    @JsonProperty("FUNC_GHU")
    private String funcGhu;

    /**
     * Original: FUNC-GN X(04) (line 20)
     * PIC clause: X(04)
     */
    @Column(name = "func_gn")
    @JsonProperty("FUNC_GN")
    private String funcGn;

    /**
     * Original: FUNC-GHN X(04) (line 21)
     * PIC clause: X(04)
     */
    @Column(name = "func_ghn")
    @JsonProperty("FUNC_GHN")
    private String funcGhn;

    /**
     * Original: FUNC-GNP X(04) (line 22)
     * PIC clause: X(04)
     */
    @Column(name = "func_gnp")
    @JsonProperty("FUNC_GNP")
    private String funcGnp;

    /**
     * Original: FUNC-GHNP X(04) (line 23)
     * PIC clause: X(04)
     */
    @Column(name = "func_ghnp")
    @JsonProperty("FUNC_GHNP")
    private String funcGhnp;

    /**
     * Original: FUNC-REPL X(04) (line 24)
     * PIC clause: X(04)
     */
    @Column(name = "func_repl")
    @JsonProperty("FUNC_REPL")
    private String funcRepl;

    /**
     * Original: FUNC-ISRT X(04) (line 25)
     * PIC clause: X(04)
     */
    @Column(name = "func_isrt")
    @JsonProperty("FUNC_ISRT")
    private String funcIsrt;

    /**
     * Original: FUNC-DLET X(04) (line 26)
     * PIC clause: X(04)
     */
    @Column(name = "func_dlet")
    @JsonProperty("FUNC_DLET")
    private String funcDlet;

    /**
     * Original: PARMCOUNT S9(05) (line 27)
     * PIC clause: S9(05)
     */
    @Column(name = "parmcount")
    @JsonProperty("PARMCOUNT")
    private Integer parmcount;

}
