package com.client.domain.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: CVCUS01Y
 * @citation cpy/CVCUS01Y.cpy
 * 18 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "cvcus01y")
public class Cvcus01y implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Original: CUST-ID 9(09) (line 5)
     * PIC clause: 9(09)
     */
    @Id
    @Column(name = "cust_id", nullable = false)
    @JsonProperty("CUST_ID")
    private Integer custId;

    /**
     * Original: CUST-FIRST-NAME X(25) (line 6)
     * PIC clause: X(25)
     */
    @Column(name = "cust_first_name")
    @JsonProperty("CUST_FIRST_NAME")
    private String custFirstName;

    /**
     * Original: CUST-MIDDLE-NAME X(25) (line 7)
     * PIC clause: X(25)
     */
    @Column(name = "cust_middle_name")
    @JsonProperty("CUST_MIDDLE_NAME")
    private String custMiddleName;

    /**
     * Original: CUST-LAST-NAME X(25) (line 8)
     * PIC clause: X(25)
     */
    @Column(name = "cust_last_name")
    @JsonProperty("CUST_LAST_NAME")
    private String custLastName;

    /**
     * Original: CUST-ADDR-LINE-1 X(50) (line 9)
     * PIC clause: X(50)
     */
    @Column(name = "cust_addr_line_1")
    @JsonProperty("CUST_ADDR_LINE_1")
    private String custAddrLine1;

    /**
     * Original: CUST-ADDR-LINE-2 X(50) (line 10)
     * PIC clause: X(50)
     */
    @Column(name = "cust_addr_line_2")
    @JsonProperty("CUST_ADDR_LINE_2")
    private String custAddrLine2;

    /**
     * Original: CUST-ADDR-LINE-3 X(50) (line 11)
     * PIC clause: X(50)
     */
    @Column(name = "cust_addr_line_3")
    @JsonProperty("CUST_ADDR_LINE_3")
    private String custAddrLine3;

    /**
     * Original: CUST-ADDR-STATE-CD X(02) (line 12)
     * PIC clause: X(02)
     */
    @Column(name = "cust_addr_state_cd")
    @JsonProperty("CUST_ADDR_STATE_CD")
    private String custAddrStateCd;

    /**
     * Original: CUST-ADDR-COUNTRY-CD X(03) (line 13)
     * PIC clause: X(03)
     */
    @Column(name = "cust_addr_country_cd")
    @JsonProperty("CUST_ADDR_COUNTRY_CD")
    private String custAddrCountryCd;

    /**
     * Original: CUST-ADDR-ZIP X(10) (line 14)
     * PIC clause: X(10)
     */
    @Column(name = "cust_addr_zip")
    @JsonProperty("CUST_ADDR_ZIP")
    private String custAddrZip;

    /**
     * Original: CUST-PHONE-NUM-1 X(15) (line 15)
     * PIC clause: X(15)
     */
    @Column(name = "cust_phone_num_1")
    @JsonProperty("CUST_PHONE_NUM_1")
    private String custPhoneNum1;

    /**
     * Original: CUST-PHONE-NUM-2 X(15) (line 16)
     * PIC clause: X(15)
     */
    @Column(name = "cust_phone_num_2")
    @JsonProperty("CUST_PHONE_NUM_2")
    private String custPhoneNum2;

    /**
     * Original: CUST-SSN 9(09) (line 17)
     * PIC clause: 9(09)
     */
    @Column(name = "cust_ssn")
    @JsonProperty("CUST_SSN")
    private Integer custSsn;

    /**
     * Original: CUST-GOVT-ISSUED-ID X(20) (line 18)
     * PIC clause: X(20)
     */
    @Column(name = "cust_govt_issued_id")
    @JsonProperty("CUST_GOVT_ISSUED_ID")
    private String custGovtIssuedId;

    /**
     * Original: CUST-DOB-YYYY-MM-DD X(10) (line 19)
     * PIC clause: X(10)
     */
    @Column(name = "cust_dob_yyyy_mm_dd")
    @JsonProperty("CUST_DOB_YYYY_MM_DD")
    private String custDobYyyyMmDd;

    /**
     * Original: CUST-EFT-ACCOUNT-ID X(10) (line 20)
     * PIC clause: X(10)
     */
    @Column(name = "cust_eft_account_id")
    @JsonProperty("CUST_EFT_ACCOUNT_ID")
    private String custEftAccountId;

    /**
     * Original: CUST-PRI-CARD-HOLDER-IND X(01) (line 21)
     * PIC clause: X(01)
     */
    @Column(name = "cust_pri_card_holder_ind")
    @JsonProperty("CUST_PRI_CARD_HOLDER_IND")
    private String custPriCardHolderInd;

    /**
     * Original: CUST-FICO-CREDIT-SCORE 9(03) (line 22)
     * PIC clause: 9(03)
     */
    @Column(name = "cust_fico_credit_score")
    @JsonProperty("CUST_FICO_CREDIT_SCORE")
    private Integer custFicoCreditScore;

}
