package com.carddemo.etl;

import java.math.BigDecimal;

/**
 * Decodes COBOL zoned-decimal (USAGE DISPLAY) fields as they appear in the
 * ASCII sample data. The sign is overpunched on the LAST byte:
 *
 * <pre>
 *   '{' = +0   'A'..'I' = +1..+9
 *   '}' = -0   'J'..'R' = -1..-9
 *   '0'..'9'   = positive digit (unsigned / already-positive)
 * </pre>
 *
 * All other bytes must be ASCII digits.
 */
public final class ZonedDecimal {

    private ZonedDecimal() {
    }

    /**
     * Parses a zoned-decimal field into a BigDecimal with the given scale
     * (implied decimal digits, e.g. scale 2 for {@code PIC S9(10)V99}).
     */
    public static BigDecimal parse(String raw, int scale) {
        if (raw == null || raw.isEmpty()) {
            throw new IllegalArgumentException("zoned decimal field is empty");
        }
        char last = raw.charAt(raw.length() - 1);
        boolean negative;
        char lastDigit;
        if (last >= '0' && last <= '9') {
            negative = false;
            lastDigit = last;
        } else if (last == '{') {
            negative = false;
            lastDigit = '0';
        } else if (last >= 'A' && last <= 'I') {
            negative = false;
            lastDigit = (char) ('1' + (last - 'A'));
        } else if (last == '}') {
            negative = true;
            lastDigit = '0';
        } else if (last >= 'J' && last <= 'R') {
            negative = true;
            lastDigit = (char) ('1' + (last - 'J'));
        } else {
            throw new IllegalArgumentException(
                    "invalid zoned-decimal sign byte '" + last + "' in field '" + raw + "'");
        }
        String digits = raw.substring(0, raw.length() - 1) + lastDigit;
        for (int i = 0; i < digits.length(); i++) {
            char c = digits.charAt(i);
            if (c < '0' || c > '9') {
                throw new IllegalArgumentException(
                        "invalid zoned-decimal digit '" + c + "' in field '" + raw + "'");
            }
        }
        BigDecimal value = new BigDecimal(digits).movePointLeft(scale).setScale(scale);
        return negative ? value.negate() : value;
    }
}
