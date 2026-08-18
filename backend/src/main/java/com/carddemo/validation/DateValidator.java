package com.carddemo.validation;

import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.Year;

/**
 * CSUTLDPY.cpy / CSUTLDTC port (plan §4 row CSUTLDTC): CCYYMMDD date edit with
 * the same accept/reject semantics and (trimmed) legacy error messages:
 * <ul>
 *   <li>year: 4-digit numeric, century 19 or 20 (EDIT-YEAR-CCYY)</li>
 *   <li>month: numeric 1–12 (EDIT-MONTH)</li>
 *   <li>day: numeric 1–31; day-31 only in 31-day months; Feb ≤ 29 and 29 only
 *       in leap years (EDIT-DAY, EDIT-DAY-MONTH-YEAR)</li>
 *   <li>final CEEDAYS call → strict java.time parse</li>
 *   <li>date-of-birth: additionally strictly before today (EDIT-DATE-OF-BIRTH)</li>
 * </ul>
 * Input accepted as YYYY-MM-DD (the UI format) or raw CCYYMMDD.
 */
public final class DateValidator {

    private DateValidator() {
    }

    /** Either {@code date} (valid) or {@code error} (message) is non-null. */
    public record Result(LocalDate date, String error) {

        public static Result ok(LocalDate date) {
            return new Result(date, null);
        }

        public static Result fail(String error) {
            return new Result(null, error);
        }
    }

    /** EDIT-DATE-CCYYMMDD: full year/month/day edit chain. */
    public static Result validate(String label, String value) {
        String raw = value == null ? "" : value.trim().replace("-", "");

        // EDIT-YEAR-CCYY
        if (raw.isEmpty()) {
            return Result.fail(label + " : Year must be supplied.");
        }
        String yy = raw.length() >= 4 ? raw.substring(0, 4) : raw;
        if (yy.length() != 4 || !isDigits(yy)) {
            return Result.fail(label + " must be 4 digit number.");
        }
        int year = Integer.parseInt(yy);
        // century must be 19 or 20
        if (!(yy.startsWith("19") || yy.startsWith("20"))) {
            return Result.fail(label + " : Century is not valid.");
        }

        // EDIT-MONTH
        String mm = raw.length() >= 6 ? raw.substring(4, 6) : raw.substring(Math.min(4, raw.length()));
        if (mm.isBlank()) {
            return Result.fail(label + " : Month must be supplied.");
        }
        if (mm.length() != 2 || !isDigits(mm)) {
            return Result.fail(label + ": Month must be a number between 1 and 12.");
        }
        int month = Integer.parseInt(mm);
        if (month < 1 || month > 12) {
            return Result.fail(label + ": Month must be a number between 1 and 12.");
        }

        // EDIT-DAY
        String dd = raw.length() >= 8 ? raw.substring(6, 8) : raw.substring(Math.min(6, raw.length()));
        if (dd.isBlank()) {
            return Result.fail(label + " : Day must be supplied.");
        }
        if (dd.length() != 2 || !isDigits(dd)) {
            return Result.fail(label + ":day must be a number between 1 and 31.");
        }
        int day = Integer.parseInt(dd);
        if (day < 1 || day > 31) {
            return Result.fail(label + ":day must be a number between 1 and 31.");
        }

        // EDIT-DAY-MONTH-YEAR: 31 only in 31-day months; February special-cased
        boolean month31 = month == 1 || month == 3 || month == 5 || month == 7
                || month == 8 || month == 10 || month == 12;
        if (day == 31 && !month31) {
            return Result.fail(label + ":Cannot have 31 days in this month.");
        }
        if (month == 2 && day == 30) {
            return Result.fail(label + ":Cannot have 30 days in this month.");
        }
        if (month == 2 && day == 29 && !Year.isLeap(year)) {
            return Result.fail(label + ":Not a leap year.Cannot have 29 days in this month.");
        }

        // CSUTLDTC (CEEDAYS) final validation → strict java.time construction
        try {
            return Result.ok(LocalDate.of(year, month, day));
        } catch (DateTimeException e) {
            return Result.fail(label + " is not a valid date");
        }
    }

    /** EDIT-DATE-OF-BIRTH: valid date AND strictly in the past. */
    public static Result validateDateOfBirth(String label, String value, LocalDate today) {
        Result r = validate(label, value);
        if (r.error() != null) {
            return r;
        }
        if (!r.date().isBefore(today)) {
            return Result.fail(label + ":cannot be in the future");
        }
        return r;
    }

    private static boolean isDigits(String s) {
        return !s.isEmpty() && s.chars().allMatch(Character::isDigit);
    }
}
