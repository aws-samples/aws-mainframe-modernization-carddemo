package com.carddemo.validation;

import com.carddemo.api.ValidationException;
import com.carddemo.api.dto.AccountUpdateRequest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

import java.math.BigDecimal;
import java.time.Clock;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.HashMap;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatCode;
import static org.assertj.core.api.Assertions.catchThrowableOfType;

/**
 * COACTUPC validation matrix (plan §7 Phase 2 exit criteria: each edit rule
 * has at least one PASS and one FAIL case; test names cite the COBOL
 * paragraph). Source: COACTUPC.cbl 1200-EDIT-MAP-INPUTS (lines 1429–1676)
 * and the shared 12xx edit primitives; dates via CSUTLDPY (EDIT-DATE-CCYYMMDD
 * / EDIT-DATE-OF-BIRTH); lookups via CSLKPCDY.
 */
class AccountUpdateValidatorTest {

    /** Fixed "today" = 2026-08-17 so DOB edits are deterministic. */
    private static final Clock CLOCK =
            Clock.fixed(Instant.parse("2026-08-17T12:00:00Z"), ZoneOffset.UTC);

    private final AccountUpdateValidator validator = new AccountUpdateValidator(CLOCK);

    // ---- baseline: a fully valid request (must pass every edit) -----------

    private static Map<String, String> validFields() {
        Map<String, String> f = new HashMap<>();
        f.put("activeStatus", "Y");
        f.put("creditLimit", "6130.00");
        f.put("cashCreditLimit", "1580.00");
        f.put("currBal", "5448.00");
        f.put("openDate", "2013-06-19");
        f.put("expirationDate", "2027-08-11");
        f.put("reissueDate", "2024-08-11");
        f.put("currCycCredit", "1912.40");
        f.put("currCycDebit", "482.15");
        f.put("groupId", "A000000000");
        f.put("firstName", "Enrico");
        f.put("middleName", "April");
        f.put("lastName", "Rosenbaum");
        f.put("ssn", "587-51-8382");
        f.put("dob", "1961-10-08");
        f.put("ficoScore", "711");
        f.put("addrLine1", "4917 Myrna Flats");
        f.put("addrLine2", "Apt. 453");
        f.put("city", "West Bernita");
        f.put("stateCd", "TX");
        f.put("zip", "75001");
        f.put("countryCd", "USA");
        f.put("phone1", "(214) 706-9510");
        f.put("phone2", "");
        f.put("govtIssuedId", "5875183820");
        f.put("eftAccountId", "0000005062");
        f.put("priCardHolder", "Y");
        return f;
    }

    private static AccountUpdateRequest req(Map<String, String> f) {
        return new AccountUpdateRequest(0L, 0L,
                f.get("activeStatus"), f.get("creditLimit"), f.get("cashCreditLimit"),
                f.get("currBal"), f.get("openDate"), f.get("expirationDate"),
                f.get("reissueDate"), f.get("currCycCredit"), f.get("currCycDebit"),
                f.get("groupId"), f.get("firstName"), f.get("middleName"),
                f.get("lastName"), f.get("ssn"), f.get("dob"), f.get("ficoScore"),
                f.get("addrLine1"), f.get("addrLine2"), f.get("city"),
                f.get("stateCd"), f.get("zip"), f.get("countryCd"),
                f.get("phone1"), f.get("phone2"), f.get("govtIssuedId"),
                f.get("eftAccountId"), f.get("priCardHolder"));
    }

    /** Runs the edit suite with one field overridden; returns the first message for that field. */
    private String failWith(String field, String value) {
        Map<String, String> f = validFields();
        f.put(field, value);
        ValidationException ex = catchThrowableOfType(
                ValidationException.class, () -> validator.validate(req(f)));
        assertThat(ex).as("edit must reject %s=%s", field, value).isNotNull();
        return ex.getFieldErrors().stream()
                .filter(e -> e.field().equals(field))
                .findFirst()
                .map(ValidationException.FieldError::message)
                .orElseThrow(() -> new AssertionError(
                        "no error recorded for " + field + ": " + ex.getFieldErrors()));
    }

    private void passesWith(String field, String value) {
        Map<String, String> f = validFields();
        f.put(field, value);
        assertThatCode(() -> validator.validate(req(f))).doesNotThrowAnyException();
    }

    @Test
    void fullyValidRequestPassesEveryEdit() {
        AccountUpdateValidator.Parsed p = validator.validate(req(validFields()));
        assertThat(p.creditLimit()).isEqualByComparingTo(new BigDecimal("6130.00"));
        assertThat(p.openDate()).isEqualTo(LocalDate.of(2013, 6, 19));
        assertThat(p.ssn()).isEqualTo("587518382");
        assertThat(p.phone1()).isEqualTo("(214)706-9510"); // legacy X(15) layout
        assertThat(p.phone2()).isNull();
        assertThat(p.stateCd()).isEqualTo("TX");
    }

    // ---- 1220-EDIT-YESNO: Account Status / Primary Card Holder ------------

    @Test
    void editYesNo_1220_accountStatusMandatoryAndYOrN() {
        assertThat(failWith("activeStatus", "")).isEqualTo("Account Status must be supplied.");
        assertThat(failWith("activeStatus", "X")).isEqualTo("Account Status must be Y or N.");
        passesWith("activeStatus", "N");
        passesWith("activeStatus", "y"); // legacy upper-cases before the compare
    }

    @Test
    void editYesNo_1220_primaryCardHolder() {
        assertThat(failWith("priCardHolder", "")).isEqualTo("Primary Card Holder must be supplied.");
        assertThat(failWith("priCardHolder", "Q")).isEqualTo("Primary Card Holder must be Y or N.");
        passesWith("priCardHolder", "N");
    }

    // ---- 1250-EDIT-SIGNED-9V2: the five signed amounts ---------------------

    @ParameterizedTest
    @ValueSource(strings = {"creditLimit", "cashCreditLimit", "currBal", "currCycCredit", "currCycDebit"})
    void editSigned9V2_1250_mandatoryAndNumeric(String field) {
        assertThat(failWith(field, "")).endsWith(" must be supplied.");
        assertThat(failWith(field, "12AB.00")).endsWith(" is not valid");
        passesWith(field, "-123.45");     // signed accepted
        passesWith(field, "1,234.56");    // TEST-NUMVAL-C accepts comma grouping
        passesWith(field, "0");
    }

    @Test
    void editSigned9V2_1250_overCapacityRejected() {
        // 9(10)V99 → max 9,999,999,999.99
        assertThat(failWith("creditLimit", "99999999999.00")).isEqualTo("Credit Limit is not valid");
    }

    // ---- EDIT-DATE-CCYYMMDD (CSUTLDPY): open/expiry/reissue dates ----------

    @ParameterizedTest
    @CsvSource({
            "openDate,   Open Date",
            "expirationDate, Expiry Date",
            "reissueDate, Reissue Date"
    })
    void editDateCcyymmdd_CSUTLDPY(String field, String label) {
        assertThat(failWith(field, "")).isEqualTo(label + " : Year must be supplied.");
        assertThat(failWith(field, "1855-01-01")).isEqualTo(label + " : Century is not valid.");
        assertThat(failWith(field, "2024-13-01"))
                .isEqualTo(label + ": Month must be a number between 1 and 12.");
        assertThat(failWith(field, "2024-04-31"))
                .isEqualTo(label + ":Cannot have 31 days in this month.");
        assertThat(failWith(field, "2023-02-29"))
                .isEqualTo(label + ":Not a leap year.Cannot have 29 days in this month.");
        passesWith(field, "2024-02-29"); // leap year OK
        passesWith(field, "2024-12-31");
    }

    // ---- 1265-EDIT-US-SSN ---------------------------------------------------

    @Test
    void editUsSsn_1265_part1ForbiddenRanges() {
        // 000 fails 1245's must-not-be-zero edit BEFORE the 1265 range check
        // (the range check only runs when FLG-EDIT-US-SSN-PART1-ISVALID)
        assertThat(failWith("ssn", "000-51-8382"))
                .isEqualTo("SSN: First 3 chars must not be zero.");
        assertThat(failWith("ssn", "666-51-8382"))
                .isEqualTo("SSN: First 3 chars: should not be 000, 666, or between 900 and 999");
        assertThat(failWith("ssn", "923-51-8382"))
                .isEqualTo("SSN: First 3 chars: should not be 000, 666, or between 900 and 999");
        passesWith("ssn", "899-51-8382");
    }

    @Test
    void editUsSsn_1265_partsNumericNonZero_via1245() {
        assertThat(failWith("ssn", "58A-51-8382")).contains("SSN: First 3 chars");
        assertThat(failWith("ssn", "587-00-8382")).isEqualTo("SSN 4th & 5th chars must not be zero.");
        assertThat(failWith("ssn", "587-51-0000")).isEqualTo("SSN Last 4 chars must not be zero.");
        assertThat(failWith("ssn", "587-51-83")).contains("SSN Last 4 chars");
        passesWith("ssn", "587518382"); // dashes optional
    }

    @Test
    void editUsSsn_nullMeansKeepStoredValue() {
        Map<String, String> f = validFields();
        f.put("ssn", null);
        AccountUpdateValidator.Parsed p = validator.validate(req(f));
        assertThat(p.ssn()).isNull();
    }

    // ---- EDIT-DATE-OF-BIRTH (CSUTLDPY): valid date AND in the past ---------

    @Test
    void editDateOfBirth_CSUTLDPY_mustBeInThePast() {
        assertThat(failWith("dob", "2030-01-01")).isEqualTo("Date of Birth:cannot be in the future");
        assertThat(failWith("dob", "2026-08-17")).isEqualTo("Date of Birth:cannot be in the future"); // today
        assertThat(failWith("dob", "1961-13-08"))
                .isEqualTo("Date of Birth: Month must be a number between 1 and 12.");
        passesWith("dob", "2026-08-16"); // yesterday (strictly before today)
    }

    // ---- 1245-EDIT-NUM-REQD + 1275-EDIT-FICO-SCORE --------------------------

    @Test
    void editFicoScore_1275_range300To850() {
        assertThat(failWith("ficoScore", "")).isEqualTo("FICO Score must be supplied.");
        assertThat(failWith("ficoScore", "29A")).isEqualTo("FICO Score must be all numeric.");
        assertThat(failWith("ficoScore", "000")).isEqualTo("FICO Score must not be zero.");
        assertThat(failWith("ficoScore", "299")).isEqualTo("FICO Score: should be between 300 and 850");
        assertThat(failWith("ficoScore", "851")).isEqualTo("FICO Score: should be between 300 and 850");
        passesWith("ficoScore", "300"); // boundary (88 FICO-RANGE-IS-VALID, COACTUPC.cbl:848)
        passesWith("ficoScore", "850");
    }

    // ---- 1225-EDIT-ALPHA-REQD: first/last name, city, country, state --------

    @ParameterizedTest
    @CsvSource({
            "firstName, First Name",
            "lastName,  Last Name",
            "city,      City",
            "countryCd, Country"
    })
    void editAlphaReqd_1225(String field, String label) {
        assertThat(failWith(field, "")).isEqualTo(label + " must be supplied.");
        assertThat(failWith(field, "Ab3")).isEqualTo(label + " can have alphabets only.");
        passesWith(field, "Van Der Berg"); // spaces allowed
    }

    // ---- 1235-EDIT-ALPHA-OPT: middle name ------------------------------------

    @Test
    void editAlphaOpt_1235_middleNameOptionalButAlphaIfSupplied() {
        passesWith("middleName", "");          // optional
        passesWith("middleName", "Lee Ann");
        assertThat(failWith("middleName", "L33"))
                .isEqualTo("Middle Name can have alphabets only.");
    }

    // ---- 1215-EDIT-MANDATORY: address line 1 ---------------------------------

    @Test
    void editMandatory_1215_addressLine1() {
        assertThat(failWith("addrLine1", "")).isEqualTo("Address Line 1 must be supplied.");
        passesWith("addrLine1", "4917 Myrna Flats #2"); // any content accepted
    }

    // ---- address line 2: optional, unvalidated -------------------------------

    @Test
    void addressLine2IsOptionalAndUnvalidated() {
        passesWith("addrLine2", "");
        passesWith("addrLine2", "Apt. #453 --");
    }

    // ---- 1270-EDIT-US-STATE-CD (CSLKPCDY VALID-US-STATE-CODE) ----------------

    @Test
    void editUsStateCd_1270_lookup() {
        assertThat(failWith("stateCd", "")).isEqualTo("State must be supplied.");
        assertThat(failWith("stateCd", "XX")).isEqualTo("State: is not a valid state code");
        Map<String, String> f = validFields();
        f.put("stateCd", "DC");
        f.put("zip", "20001"); // valid DC zip prefix
        assertThatCode(() -> validator.validate(req(f))).doesNotThrowAnyException();
    }

    // ---- 1245-EDIT-NUM-REQD: zip (len 5) and EFT account id (len 10) ---------

    @Test
    void editNumReqd_1245_zip() {
        assertThat(failWith("zip", "")).isEqualTo("Zip must be supplied.");
        assertThat(failWith("zip", "7500A")).isEqualTo("Zip must be all numeric.");
        assertThat(failWith("zip", "00000")).isEqualTo("Zip must not be zero.");
        passesWith("zip", "75001");
    }

    @Test
    void editNumReqd_1245_eftAccountId() {
        // COBOL treats EFT as NUM-REQD (1200 flow) though the mockup labels it optional
        assertThat(failWith("eftAccountId", "")).isEqualTo("EFT Account Id must be supplied.");
        assertThat(failWith("eftAccountId", "12345678AB")).isEqualTo("EFT Account Id must be all numeric.");
        assertThat(failWith("eftAccountId", "0000000000")).isEqualTo("EFT Account Id must not be zero.");
        passesWith("eftAccountId", "0000005062");
    }

    // ---- 1280-EDIT-US-STATE-ZIP-CD (CSLKPCDY combo, only if both valid) -------

    @Test
    void editUsStateZipCd_1280_combo() {
        // TX zips start 75-79 + 885; 90xxx is a CA prefix → combo invalid
        assertThat(failWith("zip", "90210")).isEqualTo("Invalid zip code for state");
        passesWith("zip", "79999");
        // cross-edit skipped when the state is itself invalid (legacy: only when
        // both fields individually pass) — expect only the state error
        Map<String, String> f = validFields();
        f.put("stateCd", "XX");
        f.put("zip", "90210");
        ValidationException ex = catchThrowableOfType(
                ValidationException.class, () -> validator.validate(req(f)));
        assertThat(ex.getFieldErrors()).extracting(ValidationException.FieldError::message)
                .contains("State: is not a valid state code")
                .doesNotContain("Invalid zip code for state");
    }

    // ---- 1260-EDIT-US-PHONE-NUM ------------------------------------------------

    @Test
    void editUsPhoneNum_1260_blankPhoneIsValid() {
        passesWith("phone1", ""); // "Not mandatory to enter a phone number"
        passesWith("phone2", "");
    }

    @Test
    void editUsPhoneNum_1260_areaCodeMustBeGeneralPurpose() {
        // 999 is not in CSLKPCDY VALID-GENERAL-PURP-CODE
        assertThat(failWith("phone1", "(999) 706-9510"))
                .isEqualTo("Phone Number 1: Not valid North America general purpose area code");
        passesWith("phone1", "(212) 706-9510");
        passesWith("phone2", "212-706-9510"); // punctuation variants accepted
    }

    @Test
    void editUsPhoneNum_1260_partEdits() {
        assertThat(failWith("phone1", "(21) 706-9510"))
                .contains("Area code");
        assertThat(failWith("phone1", "(214) 70-9510"))
                .contains("Prefix code");
        assertThat(failWith("phone1", "(214) 706-951"))
                .contains("Line number code");
        assertThat(failWith("phone2", "(214) 000-9510"))
                .isEqualTo("Phone Number 2: Prefix code cannot be zero");
        assertThat(failWith("phone2", "(214) 706-0000"))
                .isEqualTo("Phone Number 2: Line number code cannot be zero");
    }

    // ---- group id: free text, NO validation (plan §2.2 B1) ---------------------

    @Test
    void groupIdIsFreeTextWithNoValidation() {
        passesWith("groupId", "");
        passesWith("groupId", "ZZ-@#$%^&*");
    }

    // ---- multiple failures reported together ------------------------------------

    @Test
    void allFieldErrorsAreCollectedNotJustTheFirst() {
        Map<String, String> f = validFields();
        f.put("activeStatus", "X");
        f.put("ficoScore", "299");
        f.put("stateCd", "XX");
        ValidationException ex = catchThrowableOfType(
                ValidationException.class, () -> validator.validate(req(f)));
        assertThat(ex.getFieldErrors()).extracting(ValidationException.FieldError::field)
                .contains("activeStatus", "ficoScore", "stateCd");
        // legacy screen shows the FIRST failure (WS-RETURN-MSG-OFF guard)
        assertThat(ex.getMessage()).isEqualTo("Account Status must be Y or N.");
    }
}
