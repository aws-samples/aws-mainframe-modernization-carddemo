package com.carddemo.batch;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * COBOL PICTURE-clause edited-field rendering for the Phase 5 report and
 * statement layouts (CVTRA07Y / CBSTM03A STATEMENT-LINES). Only the exact
 * pictures those copybooks use are implemented:
 * <ul>
 *   <li>{@code -ZZZ,ZZZ,ZZZ.ZZ} / {@code +ZZZ,ZZZ,ZZZ.ZZ} (report amounts and
 *       totals, width 15): fixed leading sign, comma grouping, full zero
 *       suppression — a zero value renders as ALL spaces because every digit
 *       position (including the decimals) is Z.</li>
 *   <li>{@code Z(9).99-} (statement amounts, width 13): zero-suppressed
 *       integer part, decimals always shown, trailing sign — zero renders as
 *       9 spaces + {@code .00} + space.</li>
 *   <li>{@code 9(9).99-} (statement current balance, width 13): zero-padded,
 *       trailing sign.</li>
 * </ul>
 */
final class CobolPic {

    private CobolPic() {
    }

    /** PIC -ZZZ,ZZZ,ZZZ.ZZ — leading '-' or space. */
    static String minusZzz(BigDecimal v) {
        return signedZzz(v, ' ');
    }

    /** PIC +ZZZ,ZZZ,ZZZ.ZZ — leading '+' or '-'. */
    static String plusZzz(BigDecimal v) {
        return signedZzz(v, '+');
    }

    private static String signedZzz(BigDecimal v, char positiveSign) {
        BigDecimal s = v.setScale(2, RoundingMode.UNNECESSARY);
        if (s.signum() == 0) {
            // every digit position is Z -> zero suppresses the WHOLE field
            return " ".repeat(15);
        }
        char sign = s.signum() < 0 ? '-' : positiveSign;
        String digits = s.abs().unscaledValue().toString();
        String intPart = padLeftZero(digits.substring(0, digits.length() - 2), 9);
        String dec = digits.substring(digits.length() - 2);
        String grouped = intPart.substring(0, 3) + ',' + intPart.substring(3, 6)
                + ',' + intPart.substring(6, 9);
        return sign + suppress(grouped) + '.' + dec;
    }

    /** PIC Z(9).99- — decimals always shown; trailing '-' or space. */
    static String zNineV99TrailingSign(BigDecimal v) {
        BigDecimal s = v.setScale(2, RoundingMode.UNNECESSARY);
        String digits = s.abs().unscaledValue().toString();
        String intPart = padLeftZero(digits.substring(0, Math.max(0, digits.length() - 2)), 9);
        String dec = padLeftZero(digits.length() >= 2
                ? digits.substring(digits.length() - 2) : digits, 2);
        return suppress(intPart) + '.' + dec + (s.signum() < 0 ? '-' : ' ');
    }

    /** PIC 9(9).99- — zero-padded; trailing '-' or space. */
    static String nineNineV99TrailingSign(BigDecimal v) {
        BigDecimal s = v.setScale(2, RoundingMode.UNNECESSARY);
        String digits = padLeftZero(s.abs().unscaledValue().toString(), 11);
        return digits.substring(0, 9) + '.' + digits.substring(9)
                + (s.signum() < 0 ? '-' : ' ');
    }

    /** Leading zeros (and commas inside the suppressed region) become spaces. */
    private static String suppress(String grouped) {
        StringBuilder sb = new StringBuilder(grouped);
        for (int i = 0; i < sb.length(); i++) {
            char c = sb.charAt(i);
            if (c == '0' || c == ',') {
                sb.setCharAt(i, ' ');
            } else {
                break;
            }
        }
        return sb.toString();
    }

    /** MOVE to PIC X(n): left-justified, space-padded/truncated. */
    static String padX(String v, int width) {
        String s = v == null ? "" : v;
        if (s.length() >= width) {
            return s.substring(0, width);
        }
        return s + " ".repeat(width - s.length());
    }

    private static String padLeftZero(String s, int width) {
        if (s.length() >= width) {
            return s.substring(s.length() - width);
        }
        return "0".repeat(width - s.length()) + s;
    }
}
