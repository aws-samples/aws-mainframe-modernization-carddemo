package com.client.core.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Modernized JPA Entity generated from Copybook: FILE_CUSTOMER_OUTPUT
 * @citation FILE_CUSTOMER_OUTPUT (source not found)
 * 18 discrete JPA field(s) decomposed for enterprise data modeling.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "file_customer_output")
public class FileCustomerOutput implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Primary Customer Identification Number
     * PIC clause: 9(09)
     */
    @Id
    @Column(name = "cust_id", nullable = false)
    @JsonProperty("CUST_ID")
    private Integer custId;

    /**
     * Customer Legal First Name
     * PIC clause: X(25)
     */
    @Column(name = "cust_first_name")
    @JsonProperty("CUST_FIRST_NAME")
    private String custFirstName;

    /**
     * Customer Middle Name or Initial
     * PIC clause: X(25)
     */
    @Column(name = "cust_middle_name")
    @JsonProperty("CUST_MIDDLE_NAME")
    private String custMiddleName;

    /**
     * Customer Legal Last Name / Surname
     * PIC clause: X(25)
     */
    @Column(name = "cust_last_name")
    @JsonProperty("CUST_LAST_NAME")
    private String custLastName;

    /**
     * Primary Residential Street Address
     * PIC clause: X(50)
     */
    @Column(name = "cust_addr_line1")
    @JsonProperty("CUST_ADDR_LINE1")
    private String custAddrLine1;

    /**
     * Apartment, Suite, or Unit Number
     * PIC clause: X(50)
     */
    @Column(name = "cust_addr_line2")
    @JsonProperty("CUST_ADDR_LINE2")
    private String custAddrLine2;

    /**
     * Address Line 3 / Landmark
     * PIC clause: X(50)
     */
    @Column(name = "cust_addr_line3")
    @JsonProperty("CUST_ADDR_LINE3")
    private String custAddrLine3;

    /**
     * Two-Letter State Code
     * PIC clause: X(02)
     */
    @Column(name = "cust_addr_state_cd")
    @JsonProperty("CUST_ADDR_STATE_CD")
    private String custAddrStateCd;

    /**
     * ISO Country Code
     * PIC clause: X(03)
     */
    @Column(name = "cust_addr_country_cd")
    @JsonProperty("CUST_ADDR_COUNTRY_CD")
    private String custAddrCountryCd;

    /**
     * Postal Zip Code
     * PIC clause: X(10)
     */
    @Column(name = "cust_addr_zip")
    @JsonProperty("CUST_ADDR_ZIP")
    private String custAddrZip;

    /**
     * Primary Mobile Contact Number
     * PIC clause: X(15)
     */
    @Column(name = "cust_phone_num1")
    @JsonProperty("CUST_PHONE_NUM1")
    private String custPhoneNum1;

    /**
     * Secondary Contact Number
     * PIC clause: X(15)
     */
    @Column(name = "cust_phone_num2")
    @JsonProperty("CUST_PHONE_NUM2")
    private String custPhoneNum2;

    /**
     * Social Security Number
     * PIC clause: 9(09)
     */
    @Column(name = "cust_ssn")
    @JsonProperty("CUST_SSN")
    private Integer custSsn;

    /**
     * Government Issued ID / Passport
     * PIC clause: X(20)
     */
    @Column(name = "cust_govt_issued_id")
    @JsonProperty("CUST_GOVT_ISSUED_ID")
    private String custGovtIssuedId;

    /**
     * Date of Birth (YYYY-MM-DD)
     * PIC clause: X(10)
     */
    @Column(name = "cust_dob_yyyy_mm_dd")
    @JsonProperty("CUST_DOB_YYYY_MM_DD")
    private String custDobYyyyMmDd;

    /**
     * External Checking Account Reference
     * PIC clause: X(12)
     */
    @Column(name = "cust_eft_account_id")
    @JsonProperty("CUST_EFT_ACCOUNT_ID")
    private String custEftAccountId;

    /**
     * Primary Cardholder Indicator (Y/N)
     * PIC clause: X(01)
     */
    @Column(name = "cust_pri_card_holder_ind")
    @JsonProperty("CUST_PRI_CARD_HOLDER_IND")
    private String custPriCardHolderInd;

    /**
     * FICO Credit Score (300-850)
     * PIC clause: 9(03)
     */
    @Column(name = "cust_fico_credit_score")
    @JsonProperty("CUST_FICO_CREDIT_SCORE")
    private Integer custFicoCreditScore;

}
