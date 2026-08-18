package com.carddemo.validation;

import com.carddemo.api.ValidationException;
import com.carddemo.api.ValidationException.FieldError;
import com.carddemo.api.dto.AccountUpdateRequest;

import java.math.BigDecimal;
import java.time.Clock;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

/**
 * COACTUPC 1200-EDIT-MAP-INPUTS port (plan §4, R8): the full field-level edit
 * suite for the combined account+customer update, applied in the same order
 * with the same field labels and error messages as the COBOL paragraphs:
 *
 * <ul>
 *   <li>1215-EDIT-MANDATORY — non-blank</li>
 *   <li>1220-EDIT-YESNO — mandatory Y/N flag</li>
 *   <li>1225-EDIT-ALPHA-REQD — mandatory, alphabets and spaces only</li>
 *   <li>1235-EDIT-ALPHA-OPT — alpha only if supplied</li>
 *   <li>1245-EDIT-NUM-REQD — mandatory, all numeric, not zero</li>
 *   <li>1250-EDIT-SIGNED-9V2 — mandatory signed amount (TEST-NUMVAL-C)</li>
 *   <li>1260-EDIT-US-PHONE-NUM — optional; per-part area/prefix/line edits
 *       with the CSLKPCDY general-purpose area-code lookup</li>
 *   <li>1265-EDIT-US-SSN — 3 numeric parts, part1 not 000/666/900-999</li>
 *   <li>1270-EDIT-US-STATE-CD — CSLKPCDY state lookup</li>
 *   <li>1275-EDIT-FICO-SCORE — 300–850</li>
 *   <li>1280-EDIT-US-STATE-ZIP-CD — state + zip(1:2) combo lookup, only when
 *       state and zip are individually valid</li>
 *   <li>EDIT-DATE-CCYYMMDD / EDIT-DATE-OF-BIRTH — via {@link DateValidator}</li>
 * </ul>
 *
 * Group ID is deliberately NOT validated (plan §2.2 B1 — legacy free text).
 * SSN may be {@code null} meaning "keep the stored value" (the API only ever
 * returns the last 4 digits, so the client cannot echo the full value back).
 */
public final class AccountUpdateValidator {

    private static final Pattern SIGNED_9V2 =
            Pattern.compile("[+-]?\\$?(\\d{1,3}(,\\d{3})*|\\d+)(\\.\\d{1,2})?");

    private final Clock clock;

    public AccountUpdateValidator(Clock clock) {
        this.clock = clock;
    }

    /** Normalized field values after a fully clean edit pass. */
    public record Parsed(
            String activeStatus,
            BigDecimal creditLimit,
            BigDecimal cashCreditLimit,
            BigDecimal currBal,
            LocalDate openDate,
            LocalDate expirationDate,
            LocalDate reissueDate,
            BigDecimal currCycCredit,
            BigDecimal currCycDebit,
            String groupId,
            String firstName,
            String middleName,
            String lastName,
            String ssn,          // null = keep stored value
            LocalDate dob,
            Integer ficoScore,
            String addrLine1,
            String addrLine2,
            String city,
            String stateCd,
            String zip,
            String countryCd,
            String phone1,       // stored (AAA)BBB-CCCC or null
            String phone2,
            String govtIssuedId,
            String eftAccountId,
            String priCardHolder) {
    }

    /** Runs every edit; throws {@link ValidationException} carrying ALL failures. */
    public Parsed validate(AccountUpdateRequest req) {
        List<FieldError> errors = new ArrayList<>();

        // ---- 1210-EDIT-ACCOUNT (account fields, legacy order) ------------
        String activeStatus = yesNo(errors, "activeStatus", "Account Status", req.activeStatus());
        LocalDate openDate = date(errors, "openDate", "Open Date", req.openDate());
        BigDecimal creditLimit = signed9V2(errors, "creditLimit", "Credit Limit", req.creditLimit());
        LocalDate expirationDate = date(errors, "expirationDate", "Expiry Date", req.expirationDate());
        BigDecimal cashCreditLimit = signed9V2(errors, "cashCreditLimit", "Cash Credit Limit", req.cashCreditLimit());
        LocalDate reissueDate = date(errors, "reissueDate", "Reissue Date", req.reissueDate());
        BigDecimal currBal = signed9V2(errors, "currBal", "Current Balance", req.currBal());
        BigDecimal currCycCredit = signed9V2(errors, "currCycCredit", "Current Cycle Credit Limit", req.currCycCredit());
        BigDecimal currCycDebit = signed9V2(errors, "currCycDebit", "Current Cycle Debit Limit", req.currCycDebit());
        // ACCT-GROUP-ID: free text, no validation (plan §2.2 B1)
        String groupId = blankToNull(req.groupId());

        // ---- customer fields (1200 lines 1529-1676 order) -----------------
        String ssn = ssn(errors, req.ssn());
        LocalDate dob = dateOfBirth(errors, req.dob());
        Integer ficoScore = fico(errors, req.ficoScore());
        String firstName = alphaReqd(errors, "firstName", "First Name", req.firstName());
        String middleName = alphaOpt(errors, "middleName", "Middle Name", req.middleName());
        String lastName = alphaReqd(errors, "lastName", "Last Name", req.lastName());
        String addrLine1 = mandatory(errors, "addrLine1", "Address Line 1", req.addrLine1());
        String stateCd = state(errors, req.stateCd());
        String zip = numReqd(errors, "zip", "Zip", req.zip(), 5);
        String city = alphaReqd(errors, "city", "City", req.city());
        String countryCd = alphaReqd(errors, "countryCd", "Country", req.countryCd());
        String phone1 = phone(errors, "phone1", "Phone Number 1", req.phone1());
        String phone2 = phone(errors, "phone2", "Phone Number 2", req.phone2());
        String eftAccountId = numReqd(errors, "eftAccountId", "EFT Account Id", req.eftAccountId(), 10);
        String priCardHolder = yesNo(errors, "priCardHolder", "Primary Card Holder", req.priCardHolder());

        // ---- 1280-EDIT-US-STATE-ZIP-CD: only when both individually valid -
        if (stateCd != null && zip != null
                && UsLookups.STATE_CODES.contains(stateCd)
                && !UsLookups.STATE_ZIP2_COMBOS.contains(stateCd + zip.substring(0, 2))) {
            errors.add(new FieldError("zip", "Invalid zip code for state"));
        }

        if (!errors.isEmpty()) {
            throw new ValidationException(errors);
        }
        return new Parsed(activeStatus, creditLimit, cashCreditLimit, currBal,
                openDate, expirationDate, reissueDate, currCycCredit, currCycDebit,
                groupId, firstName, middleName, lastName, ssn, dob, ficoScore,
                addrLine1, blankToNull(req.addrLine2()), city, stateCd, zip, countryCd,
                phone1, phone2, blankToNull(req.govtIssuedId()), eftAccountId, priCardHolder);
    }

    // ---- edit primitives (COACTUPC 12xx paragraphs) -----------------------

    /** 1215-EDIT-MANDATORY. */
    private static String mandatory(List<FieldError> errors, String field, String label, String value) {
        if (isBlank(value)) {
            errors.add(new FieldError(field, label + " must be supplied."));
            return null;
        }
        return value.trim();
    }

    /** 1220-EDIT-YESNO. */
    private static String yesNo(List<FieldError> errors, String field, String label, String value) {
        if (isBlank(value)) {
            errors.add(new FieldError(field, label + " must be supplied."));
            return null;
        }
        String v = value.trim().toUpperCase();
        if (!v.equals("Y") && !v.equals("N")) {
            errors.add(new FieldError(field, label + " must be Y or N."));
            return null;
        }
        return v;
    }

    /** 1225-EDIT-ALPHA-REQD — alphabets and spaces only. */
    private static String alphaReqd(List<FieldError> errors, String field, String label, String value) {
        if (isBlank(value)) {
            errors.add(new FieldError(field, label + " must be supplied."));
            return null;
        }
        String v = value.trim();
        if (!v.chars().allMatch(ch -> Character.isLetter(ch) || ch == ' ')) {
            errors.add(new FieldError(field, label + " can have alphabets only."));
            return null;
        }
        return v;
    }

    /** 1235-EDIT-ALPHA-OPT. */
    private static String alphaOpt(List<FieldError> errors, String field, String label, String value) {
        if (isBlank(value)) {
            return null;
        }
        String v = value.trim();
        if (!v.chars().allMatch(ch -> Character.isLetter(ch) || ch == ' ')) {
            errors.add(new FieldError(field, label + " can have alphabets only."));
            return null;
        }
        return v;
    }

    /** 1245-EDIT-NUM-REQD — supplied, all numeric, not zero, exact length. */
    private static String numReqd(List<FieldError> errors, String field, String label, String value, int length) {
        if (isBlank(value)) {
            errors.add(new FieldError(field, label + " must be supplied."));
            return null;
        }
        String v = value.trim();
        if (v.length() != length || !v.chars().allMatch(Character::isDigit)) {
            errors.add(new FieldError(field, label + " must be all numeric."));
            return null;
        }
        if (Long.parseLong(v) == 0) {
            errors.add(new FieldError(field, label + " must not be zero."));
            return null;
        }
        return v;
    }

    /** 1250-EDIT-SIGNED-9V2 — mandatory, TEST-NUMVAL-C accepted format. */
    private static BigDecimal signed9V2(List<FieldError> errors, String field, String label, String value) {
        if (isBlank(value)) {
            errors.add(new FieldError(field, label + " must be supplied."));
            return null;
        }
        String v = value.trim();
        if (!SIGNED_9V2.matcher(v).matches()) {
            errors.add(new FieldError(field, label + " is not valid"));
            return null;
        }
        BigDecimal parsed = new BigDecimal(v.replace(",", "").replace("$", "")).setScale(2);
        // 9(10)V99 capacity
        if (parsed.abs().compareTo(new BigDecimal("9999999999.99")) > 0) {
            errors.add(new FieldError(field, label + " is not valid"));
            return null;
        }
        return parsed;
    }

    /** EDIT-DATE-CCYYMMDD (CSUTLDPY) via {@link DateValidator}. */
    private static LocalDate date(List<FieldError> errors, String field, String label, String value) {
        DateValidator.Result r = DateValidator.validate(label, value);
        if (r.error() != null) {
            errors.add(new FieldError(field, r.error()));
            return null;
        }
        return r.date();
    }

    /** EDIT-DATE-CCYYMMDD + EDIT-DATE-OF-BIRTH (strictly in the past). */
    private LocalDate dateOfBirth(List<FieldError> errors, String value) {
        DateValidator.Result r =
                DateValidator.validateDateOfBirth("Date of Birth", value, LocalDate.now(clock));
        if (r.error() != null) {
            errors.add(new FieldError("dob", r.error()));
            return null;
        }
        return r.date();
    }

    /**
     * 1265-EDIT-US-SSN: parts via 1245-EDIT-NUM-REQD; part1 additionally not
     * 000, 666 or 900–999. {@code null} means keep the stored value (the API
     * never returns the full SSN — divergence noted in the class docs).
     */
    private static String ssn(List<FieldError> errors, String value) {
        if (value == null) {
            return null; // keep stored
        }
        String digits = value.replace("-", "").trim();
        String p1 = digits.length() >= 3 ? digits.substring(0, 3) : digits;
        String p2 = digits.length() >= 5 ? digits.substring(3, 5) : "";
        String p3 = digits.length() >= 9 ? digits.substring(5, 9) : "";
        if (digits.length() != 9) {
            // decompose best-effort so part messages match the legacy labels
            p2 = digits.length() > 3 ? digits.substring(3, Math.min(5, digits.length())) : "";
            p3 = digits.length() > 5 ? digits.substring(5) : "";
        }
        int before = errors.size();
        String part1 = numReqd(errors, "ssn", "SSN: First 3 chars", p1, 3);
        if (part1 != null) {
            // range check only runs when 1245 passed (FLG-EDIT-US-SSN-PART1-ISVALID);
            // 000 is already caught by 1245's must-not-be-zero edit
            int n1 = Integer.parseInt(part1);
            if (n1 == 666 || (n1 >= 900 && n1 <= 999)) {
                errors.add(new FieldError("ssn",
                        "SSN: First 3 chars: should not be 000, 666, or between 900 and 999"));
            }
        }
        numReqd(errors, "ssn", "SSN 4th & 5th chars", p2, 2);
        numReqd(errors, "ssn", "SSN Last 4 chars", p3, 4);
        return errors.size() == before ? digits : null;
    }

    /** 1245-EDIT-NUM-REQD (3 chars) + 1275-EDIT-FICO-SCORE (300–850). */
    private static Integer fico(List<FieldError> errors, String value) {
        String v = numReqd(errors, "ficoScore", "FICO Score", value, 3);
        if (v == null) {
            return null;
        }
        int score = Integer.parseInt(v);
        if (score < 300 || score > 850) {
            errors.add(new FieldError("ficoScore", "FICO Score: should be between 300 and 850"));
            return null;
        }
        return score;
    }

    /** 1225-EDIT-ALPHA-REQD + 1270-EDIT-US-STATE-CD (CSLKPCDY lookup). */
    private static String state(List<FieldError> errors, String value) {
        String v = alphaReqd(errors, "stateCd", "State", value);
        if (v == null) {
            return null;
        }
        v = v.toUpperCase();
        if (!UsLookups.STATE_CODES.contains(v)) {
            errors.add(new FieldError("stateCd", "State: is not a valid state code"));
            return null;
        }
        return v;
    }

    /**
     * 1260-EDIT-US-PHONE-NUM: a fully blank phone is VALID (not mandatory);
     * otherwise area code (3-digit, non-zero, CSLKPCDY general-purpose code),
     * prefix (3-digit non-zero) and line number (4-digit non-zero) are all
     * edited. Stored as {@code (AAA)BBB-CCCC} (legacy X(15) layout).
     */
    private static String phone(List<FieldError> errors, String field, String label, String value) {
        if (isBlank(value)) {
            return null; // not mandatory to enter a phone number
        }
        // decompose into the legacy A/B/C parts: digit groups, or a bare
        // 10-digit string split 3/3/4 (the X(15) map layout has fixed parts)
        String[] groups = value.trim().split("\\D+");
        List<String> parts = new ArrayList<>();
        for (String g : groups) {
            if (!g.isEmpty()) {
                parts.add(g);
            }
        }
        String area = "";
        String prefix = "";
        String line = "";
        if (parts.size() == 1 && parts.get(0).length() == 10) {
            area = parts.get(0).substring(0, 3);
            prefix = parts.get(0).substring(3, 6);
            line = parts.get(0).substring(6);
        } else {
            if (!parts.isEmpty()) {
                area = parts.get(0);
            }
            if (parts.size() > 1) {
                prefix = parts.get(1);
            }
            if (parts.size() > 2) {
                line = parts.get(2);
            }
        }

        boolean ok = true;
        // EDIT-AREA-CODE
        if (area.length() != 3 || !area.chars().allMatch(Character::isDigit)) {
            errors.add(new FieldError(field, label + ": Area code must be A 3 digit number."));
            ok = false;
        } else if (Integer.parseInt(area) == 0) {
            errors.add(new FieldError(field, label + ": Area code cannot be zero"));
            ok = false;
        } else if (!UsLookups.PHONE_AREA_CODES.contains(area)) {
            errors.add(new FieldError(field,
                    label + ": Not valid North America general purpose area code"));
            ok = false;
        }
        // EDIT-US-PHONE-PREFIX
        if (prefix.length() != 3 || !prefix.chars().allMatch(Character::isDigit)) {
            errors.add(new FieldError(field, label + ": Prefix code must be A 3 digit number."));
            ok = false;
        } else if (Integer.parseInt(prefix) == 0) {
            errors.add(new FieldError(field, label + ": Prefix code cannot be zero"));
            ok = false;
        }
        // EDIT-US-PHONE-LINENUM
        if (line.length() != 4 || !line.chars().allMatch(Character::isDigit)) {
            errors.add(new FieldError(field, label + ": Line number code must be A 4 digit number."));
            ok = false;
        } else if (Integer.parseInt(line) == 0) {
            errors.add(new FieldError(field, label + ": Line number code cannot be zero"));
            ok = false;
        }
        return ok ? "(" + area + ")" + prefix + "-" + line : null;
    }

    private static boolean isBlank(String s) {
        return s == null || s.isBlank();
    }

    private static String blankToNull(String s) {
        return s == null || s.isBlank() ? null : s.trim();
    }
}
