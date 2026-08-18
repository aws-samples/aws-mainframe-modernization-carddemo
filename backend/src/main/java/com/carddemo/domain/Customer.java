package com.carddemo.domain;

import com.carddemo.crypto.SsnAttributeConverter;
import jakarta.persistence.Column;
import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Version;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

import java.time.LocalDate;

/** CUSTDAT (CVCUS01Y). SSN exposed as last-4 only via the API (plan §5). */
@Entity
@Table(name = "customers")
public class Customer {

    @Id
    @Column(name = "customer_id")
    private Long customerId;

    @Column(name = "first_name")
    private String firstName;

    @Column(name = "middle_name")
    private String middleName;

    @Column(name = "last_name")
    private String lastName;

    @Column(name = "addr_line_1")
    private String addrLine1;

    @Column(name = "addr_line_2")
    private String addrLine2;

    @Column(name = "addr_line_3")
    private String addrLine3;

    @Column(name = "addr_state_cd")
    private String addrStateCd;

    @Column(name = "addr_country_cd")
    private String addrCountryCd;

    @Column(name = "addr_zip")
    private String addrZip;

    @Column(name = "phone_num_1")
    private String phoneNum1;

    @Column(name = "phone_num_2")
    private String phoneNum2;

    /** Plaintext in the entity; AES-GCM ciphertext in the column (plan R5). */
    @Convert(converter = SsnAttributeConverter.class)
    @Column(name = "ssn")
    private String ssn;

    @Column(name = "govt_issued_id")
    private String govtIssuedId;

    @Column(name = "dob")
    private LocalDate dob;

    @Column(name = "eft_account_id")
    private String eftAccountId;

    @JdbcTypeCode(SqlTypes.CHAR)
    @Column(name = "pri_card_holder")
    private String priCardHolder;

    @JdbcTypeCode(SqlTypes.SMALLINT)
    @Column(name = "fico_score")
    private Integer ficoScore;

    @Version
    private long version;

    public Long getCustomerId() {
        return customerId;
    }

    public String getFirstName() {
        return firstName;
    }

    public String getMiddleName() {
        return middleName;
    }

    public String getLastName() {
        return lastName;
    }

    public String getAddrLine1() {
        return addrLine1;
    }

    public String getAddrLine2() {
        return addrLine2;
    }

    public String getAddrLine3() {
        return addrLine3;
    }

    public String getAddrStateCd() {
        return addrStateCd;
    }

    public String getAddrCountryCd() {
        return addrCountryCd;
    }

    public String getAddrZip() {
        return addrZip;
    }

    public String getPhoneNum1() {
        return phoneNum1;
    }

    public String getPhoneNum2() {
        return phoneNum2;
    }

    /** Full SSN — never leaves the service layer; see {@link #ssnLast4()}. */
    public String getSsn() {
        return ssn;
    }

    /** Last 4 digits of the SSN, or null (plan §5 masking rule). */
    public String ssnLast4() {
        if (ssn == null || ssn.length() < 4) {
            return null;
        }
        return ssn.substring(ssn.length() - 4);
    }

    public String getGovtIssuedId() {
        return govtIssuedId;
    }

    public LocalDate getDob() {
        return dob;
    }

    public String getEftAccountId() {
        return eftAccountId;
    }

    public String getPriCardHolder() {
        return priCardHolder;
    }

    public Integer getFicoScore() {
        return ficoScore;
    }

    public long getVersion() {
        return version;
    }

    // -- mutators for the COACTUPC update path (Phase 2, plan §4: customers
    // are edited ONLY through the account-update screen — plan §6) ---------

    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    public void setMiddleName(String middleName) {
        this.middleName = middleName;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    public void setAddrLine1(String addrLine1) {
        this.addrLine1 = addrLine1;
    }

    public void setAddrLine2(String addrLine2) {
        this.addrLine2 = addrLine2;
    }

    public void setAddrLine3(String addrLine3) {
        this.addrLine3 = addrLine3;
    }

    public void setAddrStateCd(String addrStateCd) {
        this.addrStateCd = addrStateCd;
    }

    public void setAddrCountryCd(String addrCountryCd) {
        this.addrCountryCd = addrCountryCd;
    }

    public void setAddrZip(String addrZip) {
        this.addrZip = addrZip;
    }

    public void setPhoneNum1(String phoneNum1) {
        this.phoneNum1 = phoneNum1;
    }

    public void setPhoneNum2(String phoneNum2) {
        this.phoneNum2 = phoneNum2;
    }

    public void setSsn(String ssn) {
        this.ssn = ssn;
    }

    public void setGovtIssuedId(String govtIssuedId) {
        this.govtIssuedId = govtIssuedId;
    }

    public void setDob(LocalDate dob) {
        this.dob = dob;
    }

    public void setEftAccountId(String eftAccountId) {
        this.eftAccountId = eftAccountId;
    }

    public void setPriCardHolder(String priCardHolder) {
        this.priCardHolder = priCardHolder;
    }

    public void setFicoScore(Integer ficoScore) {
        this.ficoScore = ficoScore;
    }
}
