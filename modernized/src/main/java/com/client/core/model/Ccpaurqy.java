package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: CCPAURQY
 * @citation app/app-authorization-ims-db2-mq/cpy/CCPAURQY.cpy
 * 18 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "ccpaurqy")
public class Ccpaurqy implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: PA-RQ-AUTH-DATE X(06) (line 19)
     * PIC clause: X(06)
     */
    @Column(name = "pa_rq_auth_date")
    @JsonProperty("PA_RQ_AUTH_DATE")
    private String paRqAuthDate;

    /**
     * Original: PA-RQ-AUTH-TIME X(06) (line 20)
     * PIC clause: X(06)
     */
    @Column(name = "pa_rq_auth_time")
    @JsonProperty("PA_RQ_AUTH_TIME")
    private String paRqAuthTime;

    /**
     * Original: PA-RQ-CARD-NUM X(16) (line 21)
     * PIC clause: X(16)
     */
    @Id
    @Column(name = "pa_rq_card_num", nullable = false)
    @JsonProperty("PA_RQ_CARD_NUM")
    private String paRqCardNum;

    /**
     * Original: PA-RQ-AUTH-TYPE X(04) (line 22)
     * PIC clause: X(04)
     */
    @Column(name = "pa_rq_auth_type")
    @JsonProperty("PA_RQ_AUTH_TYPE")
    private String paRqAuthType;

    /**
     * Original: PA-RQ-CARD-EXPIRY-DATE X(04) (line 23)
     * PIC clause: X(04)
     */
    @Column(name = "pa_rq_card_expiry_date")
    @JsonProperty("PA_RQ_CARD_EXPIRY_DATE")
    private String paRqCardExpiryDate;

    /**
     * Original: PA-RQ-MESSAGE-TYPE X(06) (line 24)
     * PIC clause: X(06)
     */
    @Column(name = "pa_rq_message_type")
    @JsonProperty("PA_RQ_MESSAGE_TYPE")
    private String paRqMessageType;

    /**
     * Original: PA-RQ-MESSAGE-SOURCE X(06) (line 25)
     * PIC clause: X(06)
     */
    @Column(name = "pa_rq_message_source")
    @JsonProperty("PA_RQ_MESSAGE_SOURCE")
    private String paRqMessageSource;

    /**
     * Original: PA-RQ-PROCESSING-CODE 9(06) (line 26)
     * PIC clause: 9(06)
     */
    @Column(name = "pa_rq_processing_code")
    @JsonProperty("PA_RQ_PROCESSING_CODE")
    private Integer paRqProcessingCode;

    /**
     * Original: PA-RQ-TRANSACTION-AMT +9(10) (line 27)
     * PIC clause: +9(10)
     */
    @Column(name = "pa_rq_transaction_amt")
    @JsonProperty("PA_RQ_TRANSACTION_AMT")
    private String paRqTransactionAmt;

    /**
     * Original: PA-RQ-MERCHANT-CATAGORY-CODE X(04) (line 28)
     * PIC clause: X(04)
     */
    @Column(name = "pa_rq_merchant_catagory_code")
    @JsonProperty("PA_RQ_MERCHANT_CATAGORY_CODE")
    private String paRqMerchantCatagoryCode;

    /**
     * Original: PA-RQ-ACQR-COUNTRY-CODE X(03) (line 29)
     * PIC clause: X(03)
     */
    @Column(name = "pa_rq_acqr_country_code")
    @JsonProperty("PA_RQ_ACQR_COUNTRY_CODE")
    private String paRqAcqrCountryCode;

    /**
     * Original: PA-RQ-POS-ENTRY-MODE 9(02) (line 30)
     * PIC clause: 9(02)
     */
    @Column(name = "pa_rq_pos_entry_mode")
    @JsonProperty("PA_RQ_POS_ENTRY_MODE")
    private Integer paRqPosEntryMode;

    /**
     * Original: PA-RQ-MERCHANT-ID X(15) (line 31)
     * PIC clause: X(15)
     */
    @Column(name = "pa_rq_merchant_id")
    @JsonProperty("PA_RQ_MERCHANT_ID")
    private String paRqMerchantId;

    /**
     * Original: PA-RQ-MERCHANT-NAME X(22) (line 32)
     * PIC clause: X(22)
     */
    @Column(name = "pa_rq_merchant_name")
    @JsonProperty("PA_RQ_MERCHANT_NAME")
    private String paRqMerchantName;

    /**
     * Original: PA-RQ-MERCHANT-CITY X(13) (line 33)
     * PIC clause: X(13)
     */
    @Column(name = "pa_rq_merchant_city")
    @JsonProperty("PA_RQ_MERCHANT_CITY")
    private String paRqMerchantCity;

    /**
     * Original: PA-RQ-MERCHANT-STATE X(02) (line 34)
     * PIC clause: X(02)
     */
    @Column(name = "pa_rq_merchant_state")
    @JsonProperty("PA_RQ_MERCHANT_STATE")
    private String paRqMerchantState;

    /**
     * Original: PA-RQ-MERCHANT-ZIP X(09) (line 35)
     * PIC clause: X(09)
     */
    @Column(name = "pa_rq_merchant_zip")
    @JsonProperty("PA_RQ_MERCHANT_ZIP")
    private String paRqMerchantZip;

    /**
     * Original: PA-RQ-TRANSACTION-ID X(15) (line 36)
     * PIC clause: X(15)
     */
    @Column(name = "pa_rq_transaction_id")
    @JsonProperty("PA_RQ_TRANSACTION_ID")
    private String paRqTransactionId;

}
