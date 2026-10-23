package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;
import java.math.BigDecimal;

/**
 * Modernized JPA Entity generated from Copybook: CIPAUDTY
 * @citation app/app-authorization-ims-db2-mq/cpy/CIPAUDTY.cpy
 * 27 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "cipaudty")
public class Cipaudty implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: PA-AUTH-DATE-9C S9(05) (line 20)
     * PIC clause: S9(05)
     */
    @Column(name = "pa_auth_date_9c")
    @JsonProperty("PA_AUTH_DATE_9C")
    private Integer paAuthDate9c;

    /**
     * Original: PA-AUTH-TIME-9C S9(09) (line 21)
     * PIC clause: S9(09)
     */
    @Column(name = "pa_auth_time_9c")
    @JsonProperty("PA_AUTH_TIME_9C")
    private Integer paAuthTime9c;

    /**
     * Original: PA-AUTH-ORIG-DATE X(06) (line 22)
     * PIC clause: X(06)
     */
    @Column(name = "pa_auth_orig_date")
    @JsonProperty("PA_AUTH_ORIG_DATE")
    private String paAuthOrigDate;

    /**
     * Original: PA-AUTH-ORIG-TIME X(06) (line 23)
     * PIC clause: X(06)
     */
    @Column(name = "pa_auth_orig_time")
    @JsonProperty("PA_AUTH_ORIG_TIME")
    private String paAuthOrigTime;

    /**
     * Original: PA-CARD-NUM X(16) (line 24)
     * PIC clause: X(16)
     */
    @Id
    @Column(name = "pa_card_num", nullable = false)
    @JsonProperty("PA_CARD_NUM")
    private String paCardNum;

    /**
     * Original: PA-AUTH-TYPE X(04) (line 25)
     * PIC clause: X(04)
     */
    @Column(name = "pa_auth_type")
    @JsonProperty("PA_AUTH_TYPE")
    private String paAuthType;

    /**
     * Original: PA-CARD-EXPIRY-DATE X(04) (line 26)
     * PIC clause: X(04)
     */
    @Column(name = "pa_card_expiry_date")
    @JsonProperty("PA_CARD_EXPIRY_DATE")
    private String paCardExpiryDate;

    /**
     * Original: PA-MESSAGE-TYPE X(06) (line 27)
     * PIC clause: X(06)
     */
    @Column(name = "pa_message_type")
    @JsonProperty("PA_MESSAGE_TYPE")
    private String paMessageType;

    /**
     * Original: PA-MESSAGE-SOURCE X(06) (line 28)
     * PIC clause: X(06)
     */
    @Column(name = "pa_message_source")
    @JsonProperty("PA_MESSAGE_SOURCE")
    private String paMessageSource;

    /**
     * Original: PA-AUTH-ID-CODE X(06) (line 29)
     * PIC clause: X(06)
     */
    @Column(name = "pa_auth_id_code")
    @JsonProperty("PA_AUTH_ID_CODE")
    private String paAuthIdCode;

    /**
     * Original: PA-AUTH-RESP-CODE X(02) (line 30)
     * PIC clause: X(02)
     */
    @Column(name = "pa_auth_resp_code")
    @JsonProperty("PA_AUTH_RESP_CODE")
    private String paAuthRespCode;

    /**
     * Original: PA-AUTH-RESP-REASON X(04) (line 32)
     * PIC clause: X(04)
     */
    @Column(name = "pa_auth_resp_reason")
    @JsonProperty("PA_AUTH_RESP_REASON")
    private String paAuthRespReason;

    /**
     * Original: PA-PROCESSING-CODE 9(06) (line 33)
     * PIC clause: 9(06)
     */
    @Column(name = "pa_processing_code")
    @JsonProperty("PA_PROCESSING_CODE")
    private Integer paProcessingCode;

    /**
     * Original: PA-TRANSACTION-AMT S9(10)V99 (line 34)
     * PIC clause: S9(10)V99
     */
    @Column(name = "pa_transaction_amt")
    @JsonProperty("PA_TRANSACTION_AMT")
    private BigDecimal paTransactionAmt;

    /**
     * Original: PA-APPROVED-AMT S9(10)V99 (line 35)
     * PIC clause: S9(10)V99
     */
    @Column(name = "pa_approved_amt")
    @JsonProperty("PA_APPROVED_AMT")
    private BigDecimal paApprovedAmt;

    /**
     * Original: PA-MERCHANT-CATAGORY-CODE X(04) (line 36)
     * PIC clause: X(04)
     */
    @Column(name = "pa_merchant_catagory_code")
    @JsonProperty("PA_MERCHANT_CATAGORY_CODE")
    private String paMerchantCatagoryCode;

    /**
     * Original: PA-ACQR-COUNTRY-CODE X(03) (line 37)
     * PIC clause: X(03)
     */
    @Column(name = "pa_acqr_country_code")
    @JsonProperty("PA_ACQR_COUNTRY_CODE")
    private String paAcqrCountryCode;

    /**
     * Original: PA-POS-ENTRY-MODE 9(02) (line 38)
     * PIC clause: 9(02)
     */
    @Column(name = "pa_pos_entry_mode")
    @JsonProperty("PA_POS_ENTRY_MODE")
    private Integer paPosEntryMode;

    /**
     * Original: PA-MERCHANT-ID X(15) (line 39)
     * PIC clause: X(15)
     */
    @Column(name = "pa_merchant_id")
    @JsonProperty("PA_MERCHANT_ID")
    private String paMerchantId;

    /**
     * Original: PA-MERCHANT-NAME X(22) (line 40)
     * PIC clause: X(22)
     */
    @Column(name = "pa_merchant_name")
    @JsonProperty("PA_MERCHANT_NAME")
    private String paMerchantName;

    /**
     * Original: PA-MERCHANT-CITY X(13) (line 41)
     * PIC clause: X(13)
     */
    @Column(name = "pa_merchant_city")
    @JsonProperty("PA_MERCHANT_CITY")
    private String paMerchantCity;

    /**
     * Original: PA-MERCHANT-STATE X(02) (line 42)
     * PIC clause: X(02)
     */
    @Column(name = "pa_merchant_state")
    @JsonProperty("PA_MERCHANT_STATE")
    private String paMerchantState;

    /**
     * Original: PA-MERCHANT-ZIP X(09) (line 43)
     * PIC clause: X(09)
     */
    @Column(name = "pa_merchant_zip")
    @JsonProperty("PA_MERCHANT_ZIP")
    private String paMerchantZip;

    /**
     * Original: PA-TRANSACTION-ID X(15) (line 44)
     * PIC clause: X(15)
     */
    @Column(name = "pa_transaction_id")
    @JsonProperty("PA_TRANSACTION_ID")
    private String paTransactionId;

    /**
     * Original: PA-MATCH-STATUS X(01) (line 45)
     * PIC clause: X(01)
     */
    @Column(name = "pa_match_status")
    @JsonProperty("PA_MATCH_STATUS")
    private String paMatchStatus;

    /**
     * Original: PA-AUTH-FRAUD X(01) (line 50)
     * PIC clause: X(01)
     */
    @Column(name = "pa_auth_fraud")
    @JsonProperty("PA_AUTH_FRAUD")
    private String paAuthFraud;

    /**
     * Original: PA-FRAUD-RPT-DATE X(08) (line 53)
     * PIC clause: X(08)
     */
    @Column(name = "pa_fraud_rpt_date")
    @JsonProperty("PA_FRAUD_RPT_DATE")
    private String paFraudRptDate;

}
