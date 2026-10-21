package com.carddemo.util;

import java.math.BigDecimal;

/**
 * Helpers for reading the fixed-width ASCII seed files that mirror the
 * mainframe datasets. Signed numeric fields use zoned-decimal overpunch
 * encoding on the last digit: '{'..'I' = +0..+9, '}'..'R' = -0..-9.
 */
public final class CobolFiles {

    private CobolFiles() {}

    /** 1-based inclusive substring, tolerant of short lines (pads with spaces). */
    public static String seg(String line, int start, int len) {
        int from = start - 1;
        if (from >= line.length()) {
            return " ".repeat(len);
        }
        int to = Math.min(line.length(), from + len);
        String s = line.substring(from, to);
        return s.length() < len ? s + " ".repeat(len - s.length()) : s;
    }

    public static String str(String line, int start, int len) {
        return seg(line, start, len).trim();
    }

    public static long num(String line, int start, int len) {
        String s = str(line, start, len);
        return s.isEmpty() ? 0L : Long.parseLong(s);
    }

    /**
     * Parse a signed zoned-decimal field (e.g. PIC S9(10)V99) into a
     * BigDecimal with the given number of implied decimal places.
     */
    public static BigDecimal zoned(String line, int start, int len, int scale) {
        String s = seg(line, start, len).trim();
        if (s.isEmpty()) {
            return BigDecimal.ZERO.setScale(scale);
        }
        char last = s.charAt(s.length() - 1);
        String digits = s.substring(0, s.length() - 1);
        boolean negative = false;
        int lastDigit;
        if (last >= '0' && last <= '9') {
            lastDigit = last - '0';
            digits = s;
        } else if (last == '{') {
            lastDigit = 0;
        } else if (last >= 'A' && last <= 'I') {
            lastDigit = last - 'A' + 1;
        } else if (last == '}') {
            lastDigit = 0;
            negative = true;
        } else if (last >= 'J' && last <= 'R') {
            lastDigit = last - 'J' + 1;
            negative = true;
        } else {
            // Not an overpunch digit; treat whole string as unsigned digits.
            digits = s;
            lastDigit = -1;
        }
        if (lastDigit >= 0) {
            digits = digits + lastDigit;
        }
        digits = digits.replaceAll("[^0-9]", "");
        if (digits.isEmpty()) {
            return BigDecimal.ZERO.setScale(scale);
        }
        BigDecimal v = new BigDecimal(digits).movePointLeft(scale);
        return negative ? v.negate() : v;
    }
}
