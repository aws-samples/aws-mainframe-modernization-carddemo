package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: CCPAURLY
 * @citation app/app-authorization-ims-db2-mq/cpy/CCPAURLY.cpy
 * 6 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "ccpaurly")
public class Ccpaurly implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: PA-RL-CARD-NUM X(16) (line 19)
     * PIC clause: X(16)
     */
    @Id
    @Column(name = "pa_rl_card_num", nullable = false)
    @JsonProperty("PA_RL_CARD_NUM")
    private String paRlCardNum;

    /**
     * Original: PA-RL-TRANSACTION-ID X(15) (line 20)
     * PIC clause: X(15)
     */
    @Column(name = "pa_rl_transaction_id")
    @JsonProperty("PA_RL_TRANSACTION_ID")
    private String paRlTransactionId;

    /**
     * Original: PA-RL-AUTH-ID-CODE X(06) (line 21)
     * PIC clause: X(06)
     */
    @Column(name = "pa_rl_auth_id_code")
    @JsonProperty("PA_RL_AUTH_ID_CODE")
    private String paRlAuthIdCode;

    /**
     * Original: PA-RL-AUTH-RESP-CODE X(02) (line 22)
     * PIC clause: X(02)
     */
    @Column(name = "pa_rl_auth_resp_code")
    @JsonProperty("PA_RL_AUTH_RESP_CODE")
    private String paRlAuthRespCode;

    /**
     * Original: PA-RL-AUTH-RESP-REASON X(04) (line 23)
     * PIC clause: X(04)
     */
    @Column(name = "pa_rl_auth_resp_reason")
    @JsonProperty("PA_RL_AUTH_RESP_REASON")
    private String paRlAuthRespReason;

    /**
     * Original: PA-RL-APPROVED-AMT +9(10) (line 24)
     * PIC clause: +9(10)
     */
    @Column(name = "pa_rl_approved_amt")
    @JsonProperty("PA_RL_APPROVED_AMT")
    private String paRlApprovedAmt;

}
