package com.carddemo.etl;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

/**
 * Helpers for slicing fixed-width copybook records. Offsets are 0-based byte
 * positions derived from the copybook PIC clauses. Lines shorter than the
 * copybook RECLN (trailing spaces trimmed by transfer) are treated as
 * space-padded to the right.
 */
public final class FixedWidth {

    /** DB2-style timestamp used by TRAN-ORIG-TS/TRAN-PROC-TS (PIC X(26)). */
    private static final DateTimeFormatter DB2_TS =
            DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSSSSS");

    private FixedWidth() {
    }

    /** Extracts the raw (untrimmed) field at [start, start+len), space-padding short lines. */
    public static String raw(String line, int start, int len) {
        StringBuilder sb = new StringBuilder(len);
        for (int i = start; i < start + len; i++) {
            sb.append(i < line.length() ? line.charAt(i) : ' ');
        }
        return sb.toString();
    }

    /** Trims trailing/leading spaces; all-blank becomes null (plan §2.1). */
    public static String text(String line, int start, int len) {
        String v = raw(line, start, len).trim();
        return v.isEmpty() ? null : v;
    }

    /** Unsigned display numeric (PIC 9(n)) as long. */
    public static long unsignedLong(String line, int start, int len) {
        return Long.parseLong(raw(line, start, len).trim());
    }

    /** Unsigned display numeric (PIC 9(n)) as int. */
    public static int unsignedInt(String line, int start, int len) {
        return Integer.parseInt(raw(line, start, len).trim());
    }

    /** Signed zoned decimal with implied decimals (e.g. PIC S9(10)V99 -> scale 2). */
    public static BigDecimal zoned(String line, int start, int len, int scale) {
        return ZonedDecimal.parse(raw(line, start, len), scale);
    }

    /** PIC X(10) ISO date; blank -> null; unparseable -> DateTimeParseException. */
    public static LocalDate date(String line, int start, int len) {
        String v = text(line, start, len);
        if (v == null) {
            return null;
        }
        try {
            return LocalDate.parse(v, DateTimeFormatter.ISO_LOCAL_DATE);
        } catch (DateTimeParseException e) {
            throw new DateTimeParseException(
                    "invalid date '" + v + "' at offset " + start, v, e.getErrorIndex());
        }
    }

    /** PIC X(26) DB2-format timestamp; blank -> null. */
    public static LocalDateTime timestamp(String line, int start, int len) {
        String v = text(line, start, len);
        if (v == null) {
            return null;
        }
        try {
            return LocalDateTime.parse(v, DB2_TS);
        } catch (DateTimeParseException e) {
            throw new DateTimeParseException(
                    "invalid timestamp '" + v + "' at offset " + start, v, e.getErrorIndex());
        }
    }
}
