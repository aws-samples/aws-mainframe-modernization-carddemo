package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: CSUTLDPY
 * @citation app/cpy/CSUTLDPY.cpy
 * 157 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "csutldpy")
public class Csutldpy implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * DB2 DCLGEN column SET WS
     * PIC clause: X(50)
     */
    @Column(name = "set")
    @JsonProperty("SET")
    private String set;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set2")
    @JsonProperty("SET2")
    private String set2;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if")
    @JsonProperty("IF")
    private String ifValue;

    /**
     * DB2 DCLGEN column OR WS
     * PIC clause: X(50)
     */
    @Column(name = "or")
    @JsonProperty("OR")
    private String or;

    /**
     * DB2 DCLGEN column SET INPUT
     * PIC clause: X(50)
     */
    @Column(name = "set3")
    @JsonProperty("SET3")
    private String set3;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set4")
    @JsonProperty("SET4")
    private String set4;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if2")
    @JsonProperty("IF2")
    private String ifValue2;

    /**
     * DB2 DCLGEN column FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
     * PIC clause: X(50)
     */
    @Column(name = "function")
    @JsonProperty("FUNCTION")
    private String function;

    /**
     * DB2 DCLGEN column DELIMITED BY
     * PIC clause: X(50)
     */
    @Column(name = "delimited")
    @JsonProperty("DELIMITED")
    private String delimited;

    /**
     * DB2 DCLGEN column INTO WS
     * PIC clause: X(50)
     */
    @Column(name = "into")
    @JsonProperty("INTO")
    private String into;

    /**
     * DB2 DCLGEN column GO TO
     * PIC clause: X(50)
     */
    @Column(name = "go")
    @JsonProperty("GO")
    private String go;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if3")
    @JsonProperty("IF3")
    private String ifValue3;

    /**
     * DB2 DCLGEN column SET INPUT
     * PIC clause: X(50)
     */
    @Column(name = "set5")
    @JsonProperty("SET5")
    private String set5;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set6")
    @JsonProperty("SET6")
    private String set6;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if4")
    @JsonProperty("IF4")
    private String ifValue4;

    /**
     * DB2 DCLGEN column FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
     * PIC clause: X(50)
     */
    @Column(name = "function2")
    @JsonProperty("FUNCTION2")
    private String function2;

    /**
     * DB2 DCLGEN column DELIMITED BY
     * PIC clause: X(50)
     */
    @Column(name = "delimited2")
    @JsonProperty("DELIMITED2")
    private String delimited2;

    /**
     * DB2 DCLGEN column INTO WS
     * PIC clause: X(50)
     */
    @Column(name = "into2")
    @JsonProperty("INTO2")
    private String into2;

    /**
     * DB2 DCLGEN column GO TO
     * PIC clause: X(50)
     */
    @Column(name = "go2")
    @JsonProperty("GO2")
    private String go2;

    /**
     * DB2 DCLGEN column IF THIS
     * PIC clause: X(50)
     */
    @Column(name = "if5")
    @JsonProperty("IF5")
    private String ifValue5;

    /**
     * DB2 DCLGEN column OR LAST
     * PIC clause: X(50)
     */
    @Column(name = "or2")
    @JsonProperty("OR2")
    private String or2;

    /**
     * DB2 DCLGEN column SET INPUT
     * PIC clause: X(50)
     */
    @Column(name = "set7")
    @JsonProperty("SET7")
    private String set7;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set8")
    @JsonProperty("SET8")
    private String set8;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if6")
    @JsonProperty("IF6")
    private String ifValue6;

    /**
     * DB2 DCLGEN column FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
     * PIC clause: X(50)
     */
    @Column(name = "function3")
    @JsonProperty("FUNCTION3")
    private String function3;

    /**
     * DB2 DCLGEN column DELIMITED BY
     * PIC clause: X(50)
     */
    @Column(name = "delimited3")
    @JsonProperty("DELIMITED3")
    private String delimited3;

    /**
     * DB2 DCLGEN column INTO WS
     * PIC clause: X(50)
     */
    @Column(name = "into3")
    @JsonProperty("INTO3")
    private String into3;

    /**
     * DB2 DCLGEN column GO TO
     * PIC clause: X(50)
     */
    @Column(name = "go3")
    @JsonProperty("GO3")
    private String go3;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set9")
    @JsonProperty("SET9")
    private String set9;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set10")
    @JsonProperty("SET10")
    private String set10;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if7")
    @JsonProperty("IF7")
    private String ifValue7;

    /**
     * DB2 DCLGEN column OR WS
     * PIC clause: X(50)
     */
    @Column(name = "or3")
    @JsonProperty("OR3")
    private String or3;

    /**
     * DB2 DCLGEN column SET INPUT
     * PIC clause: X(50)
     */
    @Column(name = "set11")
    @JsonProperty("SET11")
    private String set11;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set12")
    @JsonProperty("SET12")
    private String set12;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if8")
    @JsonProperty("IF8")
    private String ifValue8;

    /**
     * DB2 DCLGEN column FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
     * PIC clause: X(50)
     */
    @Column(name = "function4")
    @JsonProperty("FUNCTION4")
    private String function4;

    /**
     * DB2 DCLGEN column DELIMITED BY
     * PIC clause: X(50)
     */
    @Column(name = "delimited4")
    @JsonProperty("DELIMITED4")
    private String delimited4;

    /**
     * DB2 DCLGEN column INTO WS
     * PIC clause: X(50)
     */
    @Column(name = "into4")
    @JsonProperty("INTO4")
    private String into4;

    /**
     * DB2 DCLGEN column GO TO
     * PIC clause: X(50)
     */
    @Column(name = "go4")
    @JsonProperty("GO4")
    private String go4;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if9")
    @JsonProperty("IF9")
    private String ifValue9;

    /**
     * DB2 DCLGEN column SET INPUT
     * PIC clause: X(50)
     */
    @Column(name = "set13")
    @JsonProperty("SET13")
    private String set13;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set14")
    @JsonProperty("SET14")
    private String set14;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if10")
    @JsonProperty("IF10")
    private String ifValue10;

    /**
     * DB2 DCLGEN column FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
     * PIC clause: X(50)
     */
    @Column(name = "function5")
    @JsonProperty("FUNCTION5")
    private String function5;

    /**
     * DB2 DCLGEN column DELIMITED BY
     * PIC clause: X(50)
     */
    @Column(name = "delimited5")
    @JsonProperty("DELIMITED5")
    private String delimited5;

    /**
     * DB2 DCLGEN column INTO WS
     * PIC clause: X(50)
     */
    @Column(name = "into5")
    @JsonProperty("INTO5")
    private String into5;

    /**
     * DB2 DCLGEN column GO TO
     * PIC clause: X(50)
     */
    @Column(name = "go5")
    @JsonProperty("GO5")
    private String go5;

    /**
     * DB2 DCLGEN column IF FUNCTION
     * PIC clause: X(50)
     */
    @Column(name = "if11")
    @JsonProperty("IF11")
    private String ifValue11;

    /**
     * DB2 DCLGEN column COMPUTE WS
     * PIC clause: X(50)
     */
    @Column(name = "compute")
    @JsonProperty("COMPUTE")
    private String compute;

    /**
     * DB2 DCLGEN column SET INPUT
     * PIC clause: X(50)
     */
    @Column(name = "set15")
    @JsonProperty("SET15")
    private String set15;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set16")
    @JsonProperty("SET16")
    private String set16;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if12")
    @JsonProperty("IF12")
    private String ifValue12;

    /**
     * DB2 DCLGEN column FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
     * PIC clause: X(50)
     */
    @Column(name = "function6")
    @JsonProperty("FUNCTION6")
    private String function6;

    /**
     * DB2 DCLGEN column DELIMITED BY
     * PIC clause: X(50)
     */
    @Column(name = "delimited6")
    @JsonProperty("DELIMITED6")
    private String delimited6;

    /**
     * DB2 DCLGEN column INTO WS
     * PIC clause: X(50)
     */
    @Column(name = "into6")
    @JsonProperty("INTO6")
    private String into6;

    /**
     * DB2 DCLGEN column GO TO
     * PIC clause: X(50)
     */
    @Column(name = "go6")
    @JsonProperty("GO6")
    private String go6;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set17")
    @JsonProperty("SET17")
    private String set17;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set18")
    @JsonProperty("SET18")
    private String set18;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if13")
    @JsonProperty("IF13")
    private String ifValue13;

    /**
     * DB2 DCLGEN column OR WS
     * PIC clause: X(50)
     */
    @Column(name = "or4")
    @JsonProperty("OR4")
    private String or4;

    /**
     * DB2 DCLGEN column SET INPUT
     * PIC clause: X(50)
     */
    @Column(name = "set19")
    @JsonProperty("SET19")
    private String set19;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set20")
    @JsonProperty("SET20")
    private String set20;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if14")
    @JsonProperty("IF14")
    private String ifValue14;

    /**
     * DB2 DCLGEN column FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
     * PIC clause: X(50)
     */
    @Column(name = "function7")
    @JsonProperty("FUNCTION7")
    private String function7;

    /**
     * DB2 DCLGEN column DELIMITED BY
     * PIC clause: X(50)
     */
    @Column(name = "delimited7")
    @JsonProperty("DELIMITED7")
    private String delimited7;

    /**
     * DB2 DCLGEN column INTO WS
     * PIC clause: X(50)
     */
    @Column(name = "into7")
    @JsonProperty("INTO7")
    private String into7;

    /**
     * DB2 DCLGEN column GO TO
     * PIC clause: X(50)
     */
    @Column(name = "go7")
    @JsonProperty("GO7")
    private String go7;

    /**
     * DB2 DCLGEN column IF FUNCTION
     * PIC clause: X(50)
     */
    @Column(name = "if15")
    @JsonProperty("IF15")
    private String ifValue15;

    /**
     * DB2 DCLGEN column COMPUTE WS
     * PIC clause: X(50)
     */
    @Column(name = "compute2")
    @JsonProperty("COMPUTE2")
    private String compute2;

    /**
     * DB2 DCLGEN column SET INPUT
     * PIC clause: X(50)
     */
    @Column(name = "set21")
    @JsonProperty("SET21")
    private String set21;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set22")
    @JsonProperty("SET22")
    private String set22;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if16")
    @JsonProperty("IF16")
    private String ifValue16;

    /**
     * DB2 DCLGEN column FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
     * PIC clause: X(50)
     */
    @Column(name = "function8")
    @JsonProperty("FUNCTION8")
    private String function8;

    /**
     * DB2 DCLGEN column DELIMITED BY
     * PIC clause: X(50)
     */
    @Column(name = "delimited8")
    @JsonProperty("DELIMITED8")
    private String delimited8;

    /**
     * DB2 DCLGEN column INTO WS
     * PIC clause: X(50)
     */
    @Column(name = "into8")
    @JsonProperty("INTO8")
    private String into8;

    /**
     * DB2 DCLGEN column GO TO
     * PIC clause: X(50)
     */
    @Column(name = "go8")
    @JsonProperty("GO8")
    private String go8;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if17")
    @JsonProperty("IF17")
    private String ifValue17;

    /**
     * DB2 DCLGEN column SET INPUT
     * PIC clause: X(50)
     */
    @Column(name = "set23")
    @JsonProperty("SET23")
    private String set23;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set24")
    @JsonProperty("SET24")
    private String set24;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if18")
    @JsonProperty("IF18")
    private String ifValue18;

    /**
     * DB2 DCLGEN column FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
     * PIC clause: X(50)
     */
    @Column(name = "function9")
    @JsonProperty("FUNCTION9")
    private String function9;

    /**
     * DB2 DCLGEN column DELIMITED BY
     * PIC clause: X(50)
     */
    @Column(name = "delimited9")
    @JsonProperty("DELIMITED9")
    private String delimited9;

    /**
     * DB2 DCLGEN column INTO WS
     * PIC clause: X(50)
     */
    @Column(name = "into9")
    @JsonProperty("INTO9")
    private String into9;

    /**
     * DB2 DCLGEN column GO TO
     * PIC clause: X(50)
     */
    @Column(name = "go9")
    @JsonProperty("GO9")
    private String go9;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set25")
    @JsonProperty("SET25")
    private String set25;

    /**
     * DB2 DCLGEN column IF NOT
     * PIC clause: X(50)
     */
    @Column(name = "if19")
    @JsonProperty("IF19")
    private String ifValue19;

    /**
     * DB2 DCLGEN column AND WS
     * PIC clause: X(50)
     */
    @Column(name = "and")
    @JsonProperty("AND")
    private String and;

    /**
     * DB2 DCLGEN column SET INPUT
     * PIC clause: X(50)
     */
    @Column(name = "set26")
    @JsonProperty("SET26")
    private String set26;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set27")
    @JsonProperty("SET27")
    private String set27;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set28")
    @JsonProperty("SET28")
    private String set28;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if20")
    @JsonProperty("IF20")
    private String ifValue20;

    /**
     * DB2 DCLGEN column FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
     * PIC clause: X(50)
     */
    @Column(name = "function10")
    @JsonProperty("FUNCTION10")
    private String function10;

    /**
     * DB2 DCLGEN column DELIMITED BY
     * PIC clause: X(50)
     */
    @Column(name = "delimited10")
    @JsonProperty("DELIMITED10")
    private String delimited10;

    /**
     * DB2 DCLGEN column INTO WS
     * PIC clause: X(50)
     */
    @Column(name = "into10")
    @JsonProperty("INTO10")
    private String into10;

    /**
     * DB2 DCLGEN column GO TO
     * PIC clause: X(50)
     */
    @Column(name = "go10")
    @JsonProperty("GO10")
    private String go10;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if21")
    @JsonProperty("IF21")
    private String ifValue21;

    /**
     * DB2 DCLGEN column AND WS
     * PIC clause: X(50)
     */
    @Column(name = "and2")
    @JsonProperty("AND2")
    private String and2;

    /**
     * DB2 DCLGEN column SET INPUT
     * PIC clause: X(50)
     */
    @Column(name = "set29")
    @JsonProperty("SET29")
    private String set29;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set30")
    @JsonProperty("SET30")
    private String set30;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set31")
    @JsonProperty("SET31")
    private String set31;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if22")
    @JsonProperty("IF22")
    private String ifValue22;

    /**
     * DB2 DCLGEN column FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
     * PIC clause: X(50)
     */
    @Column(name = "function11")
    @JsonProperty("FUNCTION11")
    private String function11;

    /**
     * DB2 DCLGEN column DELIMITED BY
     * PIC clause: X(50)
     */
    @Column(name = "delimited11")
    @JsonProperty("DELIMITED11")
    private String delimited11;

    /**
     * DB2 DCLGEN column INTO WS
     * PIC clause: X(50)
     */
    @Column(name = "into11")
    @JsonProperty("INTO11")
    private String into11;

    /**
     * DB2 DCLGEN column GO TO
     * PIC clause: X(50)
     */
    @Column(name = "go11")
    @JsonProperty("GO11")
    private String go11;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if23")
    @JsonProperty("IF23")
    private String ifValue23;

    /**
     * DB2 DCLGEN column AND WS
     * PIC clause: X(50)
     */
    @Column(name = "and3")
    @JsonProperty("AND3")
    private String and3;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if24")
    @JsonProperty("IF24")
    private String ifValue24;

    /**
     * DB2 DCLGEN column MOVE 400
     * PIC clause: X(50)
     */
    @Column(name = "move")
    @JsonProperty("MOVE")
    private String move;

    /**
     * DB2 DCLGEN column MOVE 4
     * PIC clause: X(50)
     */
    @Column(name = "move2")
    @JsonProperty("MOVE2")
    private String move2;

    /**
     * DB2 DCLGEN column DIVIDE WS
     * PIC clause: X(50)
     */
    @Id
    @Column(name = "divide", nullable = false)
    @JsonProperty("DIVIDE")
    private String divide;

    /**
     * DB2 DCLGEN column BY WS
     * PIC clause: X(50)
     */
    @Column(name = "by")
    @JsonProperty("BY")
    private String by;

    /**
     * DB2 DCLGEN column GIVING WS
     * PIC clause: X(50)
     */
    @Column(name = "giving")
    @JsonProperty("GIVING")
    private String giving;

    /**
     * DB2 DCLGEN column REMAINDER WS
     * PIC clause: X(50)
     */
    @Column(name = "remainder")
    @JsonProperty("REMAINDER")
    private String remainder;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if25")
    @JsonProperty("IF25")
    private String ifValue25;

    /**
     * DB2 DCLGEN column SET INPUT
     * PIC clause: X(50)
     */
    @Column(name = "set32")
    @JsonProperty("SET32")
    private String set32;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set33")
    @JsonProperty("SET33")
    private String set33;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set34")
    @JsonProperty("SET34")
    private String set34;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set35")
    @JsonProperty("SET35")
    private String set35;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if26")
    @JsonProperty("IF26")
    private String ifValue26;

    /**
     * DB2 DCLGEN column FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
     * PIC clause: X(50)
     */
    @Column(name = "function12")
    @JsonProperty("FUNCTION12")
    private String function12;

    /**
     * DB2 DCLGEN column DELIMITED BY
     * PIC clause: X(50)
     */
    @Column(name = "delimited12")
    @JsonProperty("DELIMITED12")
    private String delimited12;

    /**
     * DB2 DCLGEN column INTO WS
     * PIC clause: X(50)
     */
    @Column(name = "into12")
    @JsonProperty("INTO12")
    private String into12;

    /**
     * DB2 DCLGEN column GO TO
     * PIC clause: X(50)
     */
    @Column(name = "go12")
    @JsonProperty("GO12")
    private String go12;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if27")
    @JsonProperty("IF27")
    private String ifValue27;

    /**
     * DB2 DCLGEN column GO TO
     * PIC clause: X(50)
     */
    @Column(name = "go13")
    @JsonProperty("GO13")
    private String go13;

    /**
     * DB2 DCLGEN column INITIALIZE WS
     * PIC clause: X(50)
     */
    @Column(name = "initialize")
    @JsonProperty("INITIALIZE")
    private String initialize;

    /**
     * DB2 DCLGEN column 005100 CALL
     * PIC clause: X(50)
     */
    @Column(name = "005100")
    @JsonProperty("005100")
    private String m005100;

    /**
     * DB2 DCLGEN column USING WS
     * PIC clause: X(50)
     */
    @Column(name = "using")
    @JsonProperty("USING")
    private String using;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if28")
    @JsonProperty("IF28")
    private String ifValue28;

    /**
     * DB2 DCLGEN column SET INPUT
     * PIC clause: X(50)
     */
    @Column(name = "set36")
    @JsonProperty("SET36")
    private String set36;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set37")
    @JsonProperty("SET37")
    private String set37;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set38")
    @JsonProperty("SET38")
    private String set38;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set39")
    @JsonProperty("SET39")
    private String set39;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if29")
    @JsonProperty("IF29")
    private String ifValue29;

    /**
     * DB2 DCLGEN column FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
     * PIC clause: X(50)
     */
    @Column(name = "function13")
    @JsonProperty("FUNCTION13")
    private String function13;

    /**
     * DB2 DCLGEN column DELIMITED BY
     * PIC clause: X(50)
     */
    @Column(name = "delimited13")
    @JsonProperty("DELIMITED13")
    private String delimited13;

    /**
     * DB2 DCLGEN column INTO WS
     * PIC clause: X(50)
     */
    @Column(name = "into13")
    @JsonProperty("INTO13")
    private String into13;

    /**
     * DB2 DCLGEN column GO TO
     * PIC clause: X(50)
     */
    @Column(name = "go14")
    @JsonProperty("GO14")
    private String go14;

    /**
     * DB2 DCLGEN column IF NOT
     * PIC clause: X(50)
     */
    @Column(name = "if30")
    @JsonProperty("IF30")
    private String ifValue30;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set40")
    @JsonProperty("SET40")
    private String set40;

    /**
     * DB2 DCLGEN column SET WS
     * PIC clause: X(50)
     */
    @Column(name = "set41")
    @JsonProperty("SET41")
    private String set41;

    /**
     * DB2 DCLGEN column MOVE FUNCTION
     * PIC clause: X(50)
     */
    @Column(name = "move3")
    @JsonProperty("MOVE3")
    private String move3;

    /**
     * DB2 DCLGEN column COMPUTE WS
     * PIC clause: X(50)
     */
    @Column(name = "compute3")
    @JsonProperty("COMPUTE3")
    private String compute3;

    /**
     * DB2 DCLGEN column FUNCTION INTEGER
     * PIC clause: S9(9) COMP
     */
    @Column(name = "function14")
    @JsonProperty("FUNCTION14")
    private Integer function14;

    /**
     * DB2 DCLGEN column COMPUTE WS
     * PIC clause: X(50)
     */
    @Column(name = "compute4")
    @JsonProperty("COMPUTE4")
    private String compute4;

    /**
     * DB2 DCLGEN column FUNCTION INTEGER
     * PIC clause: S9(9) COMP
     */
    @Column(name = "function15")
    @JsonProperty("FUNCTION15")
    private Integer function15;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if31")
    @JsonProperty("IF31")
    private String ifValue31;

    /**
     * DB2 DCLGEN column SET INPUT
     * PIC clause: X(50)
     */
    @Column(name = "set42")
    @JsonProperty("SET42")
    private String set42;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set43")
    @JsonProperty("SET43")
    private String set43;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set44")
    @JsonProperty("SET44")
    private String set44;

    /**
     * DB2 DCLGEN column SET FLG
     * PIC clause: X(50)
     */
    @Column(name = "set45")
    @JsonProperty("SET45")
    private String set45;

    /**
     * DB2 DCLGEN column IF WS
     * PIC clause: X(50)
     */
    @Column(name = "if32")
    @JsonProperty("IF32")
    private String ifValue32;

    /**
     * DB2 DCLGEN column FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
     * PIC clause: X(50)
     */
    @Column(name = "function16")
    @JsonProperty("FUNCTION16")
    private String function16;

    /**
     * DB2 DCLGEN column DELIMITED BY
     * PIC clause: X(50)
     */
    @Column(name = "delimited14")
    @JsonProperty("DELIMITED14")
    private String delimited14;

    /**
     * DB2 DCLGEN column INTO WS
     * PIC clause: X(50)
     */
    @Column(name = "into14")
    @JsonProperty("INTO14")
    private String into14;

    /**
     * DB2 DCLGEN column GO TO
     * PIC clause: X(50)
     */
    @Column(name = "go15")
    @JsonProperty("GO15")
    private String go15;

}
