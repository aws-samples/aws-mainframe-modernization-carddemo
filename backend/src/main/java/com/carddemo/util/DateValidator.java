package com.carddemo.util;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.ResolverStyle;

/**
 * Port of CSUTLDTC.cbl: validates a date string against the strict
 * YYYY-MM-DD Gregorian calendar (the CEEDAYS call in the original).
 */
public final class DateValidator {

    private static final DateTimeFormatter FMT =
            DateTimeFormatter.ofPattern("uuuu-MM-dd").withResolverStyle(ResolverStyle.STRICT);

    private DateValidator() {}

    /** True when the value is a real calendar date in YYYY-MM-DD form. */
    public static boolean isValid(String date) {
        if (date == null || !date.matches("\\d{4}-\\d{2}-\\d{2}")) {
            return false;
        }
        try {
            LocalDate.parse(date, FMT);
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    public static LocalDate parse(String date) {
        return LocalDate.parse(date, FMT);
    }
}
