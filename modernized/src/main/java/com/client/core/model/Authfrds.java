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
 * Modernized JPA Entity generated from Copybook: AUTHFRDS
 * @citation app/app-authorization-ims-db2-mq/dcl/AUTHFRDS.dcl
 * 27 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "authfrds")
public class Authfrds implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: CARD-NUM X(16) (line 56)
     * PIC clause: X(16)
     */
    @Id
    @Column(name = "card_num", nullable = false)
    @JsonProperty("CARD_NUM")
    private String cardNum;

    /**
     * Original: AUTH-TS X(26) (line 57)
     * PIC clause: X(26)
     */
    @Column(name = "auth_ts")
    @JsonProperty("AUTH_TS")
    private String authTs;

    /**
     * Original: AUTH-TYPE X(4) (line 58)
     * PIC clause: X(4)
     */
    @Column(name = "auth_type")
    @JsonProperty("AUTH_TYPE")
    private String authType;

    /**
     * Original: CARD-EXPIRY-DATE X(4) (line 59)
     * PIC clause: X(4)
     */
    @Column(name = "card_expiry_date")
    @JsonProperty("CARD_EXPIRY_DATE")
    private String cardExpiryDate;

    /**
     * Original: MESSAGE-TYPE X(6) (line 60)
     * PIC clause: X(6)
     */
    @Column(name = "message_type")
    @JsonProperty("MESSAGE_TYPE")
    private String messageType;

    /**
     * Original: MESSAGE-SOURCE X(6) (line 61)
     * PIC clause: X(6)
     */
    @Column(name = "message_source")
    @JsonProperty("MESSAGE_SOURCE")
    private String messageSource;

    /**
     * Original: AUTH-ID-CODE X(6) (line 62)
     * PIC clause: X(6)
     */
    @Column(name = "auth_id_code")
    @JsonProperty("AUTH_ID_CODE")
    private String authIdCode;

    /**
     * Original: AUTH-RESP-CODE X(2) (line 63)
     * PIC clause: X(2)
     */
    @Column(name = "auth_resp_code")
    @JsonProperty("AUTH_RESP_CODE")
    private String authRespCode;

    /**
     * Original: AUTH-RESP-REASON X(4) (line 64)
     * PIC clause: X(4)
     */
    @Column(name = "auth_resp_reason")
    @JsonProperty("AUTH_RESP_REASON")
    private String authRespReason;

    /**
     * Original: PROCESSING-CODE X(6) (line 65)
     * PIC clause: X(6)
     */
    @Column(name = "processing_code")
    @JsonProperty("PROCESSING_CODE")
    private String processingCode;

    /**
     * Original: TRANSACTION-AMT S9(10)V9(2) (line 66)
     * PIC clause: S9(10)V9(2)
     */
    @Column(name = "transaction_amt")
    @JsonProperty("TRANSACTION_AMT")
    private BigDecimal transactionAmt;

    /**
     * Original: APPROVED-AMT S9(10)V9(2) (line 67)
     * PIC clause: S9(10)V9(2)
     */
    @Column(name = "approved_amt")
    @JsonProperty("APPROVED_AMT")
    private BigDecimal approvedAmt;

    /**
     * Original: MERCHANT-CATAGORY-CODE X(4) (line 68)
     * PIC clause: X(4)
     */
    @Column(name = "merchant_catagory_code")
    @JsonProperty("MERCHANT_CATAGORY_CODE")
    private String merchantCatagoryCode;

    /**
     * Original: ACQR-COUNTRY-CODE X(3) (line 70)
     * PIC clause: X(3)
     */
    @Column(name = "acqr_country_code")
    @JsonProperty("ACQR_COUNTRY_CODE")
    private String acqrCountryCode;

    /**
     * Original: POS-ENTRY-MODE S9(4) (line 71)
     * PIC clause: S9(4)
     */
    @Column(name = "pos_entry_mode")
    @JsonProperty("POS_ENTRY_MODE")
    private Integer posEntryMode;

    /**
     * Original: MERCHANT-ID X(15) (line 72)
     * PIC clause: X(15)
     */
    @Column(name = "merchant_id")
    @JsonProperty("MERCHANT_ID")
    private String merchantId;

    /**
     * Original: MERCHANT-NAME-LEN S9(4) (line 74)
     * PIC clause: S9(4)
     */
    @Column(name = "merchant_name_len")
    @JsonProperty("MERCHANT_NAME_LEN")
    private Integer merchantNameLen;

    /**
     * Original: MERCHANT-NAME-TEXT X(22) (line 76)
     * PIC clause: X(22)
     */
    @Column(name = "merchant_name_text")
    @JsonProperty("MERCHANT_NAME_TEXT")
    private String merchantNameText;

    /**
     * Original: MERCHANT-CITY X(13) (line 78)
     * PIC clause: X(13)
     */
    @Column(name = "merchant_city")
    @JsonProperty("MERCHANT_CITY")
    private String merchantCity;

    /**
     * Original: MERCHANT-STATE X(2) (line 79)
     * PIC clause: X(2)
     */
    @Column(name = "merchant_state")
    @JsonProperty("MERCHANT_STATE")
    private String merchantState;

    /**
     * Original: MERCHANT-ZIP X(9) (line 80)
     * PIC clause: X(9)
     */
    @Column(name = "merchant_zip")
    @JsonProperty("MERCHANT_ZIP")
    private String merchantZip;

    /**
     * Original: TRANSACTION-ID X(15) (line 81)
     * PIC clause: X(15)
     */
    @Column(name = "transaction_id")
    @JsonProperty("TRANSACTION_ID")
    private String transactionId;

    /**
     * Original: MATCH-STATUS X(1) (line 82)
     * PIC clause: X(1)
     */
    @Column(name = "match_status")
    @JsonProperty("MATCH_STATUS")
    private String matchStatus;

    /**
     * Original: AUTH-FRAUD X(1) (line 83)
     * PIC clause: X(1)
     */
    @Column(name = "auth_fraud")
    @JsonProperty("AUTH_FRAUD")
    private String authFraud;

    /**
     * Original: FRAUD-RPT-DATE X(10) (line 84)
     * PIC clause: X(10)
     */
    @Column(name = "fraud_rpt_date")
    @JsonProperty("FRAUD_RPT_DATE")
    private String fraudRptDate;

    /**
     * Original: ACCT-ID S9(11)V (line 85)
     * PIC clause: S9(11)V
     */
    @Column(name = "acct_id")
    @JsonProperty("ACCT_ID")
    private BigDecimal acctId;

    /**
     * Original: CUST-ID S9(9)V (line 86)
     * PIC clause: S9(9)V
     */
    @Column(name = "cust_id")
    @JsonProperty("CUST_ID")
    private BigDecimal custId;

}
