package com.carddemo.batch;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.math.RoundingMode;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Pins CBACT04C's interest arithmetic (plan R1, §8 "bal*rate/1200 rounding"):
 * {@code COMPUTE WS-MONTHLY-INT = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200} has
 * NO ROUNDED clause — COBOL truncates toward zero at the receiving field's
 * scale (2), i.e. {@link RoundingMode#DOWN}, never HALF_UP.
 */
class InterestRoundingTest {

    private static BigDecimal interest(String balance, String rate) {
        return InterestCalcTasklet.monthlyInterest(new BigDecimal(balance), new BigDecimal(rate));
    }

    @Test
    void truncatesTowardZeroWhereHalfUpWouldRoundUp() {
        // 1164.87 * 15.00 / 1200 = 14.560875 → 14.56 either way, but
        // 999.99 * 1.50 / 1200 = 1.2499875 → DOWN 1.24 (HALF_UP would differ at .5+)
        // pick values where HALF_UP and DOWN diverge:
        // 100.00 * 1.55 / 1200 = 0.12916... → 0.12 (HALF_UP: 0.13)
        assertThat(interest("100.00", "1.55")).isEqualByComparingTo("0.12");
        assertThat(new BigDecimal("100.00").multiply(new BigDecimal("1.55"))
                .divide(InterestCalcTasklet.TWELVE_HUNDRED, 2, RoundingMode.HALF_UP))
                .isEqualByComparingTo("0.13"); // proves the case discriminates
    }

    @Test
    void truncatesTowardZeroForNegativeBalances() {
        // -100.00 * 1.55 / 1200 = -0.12916... → truncate toward zero → -0.12
        // (FLOOR would give -0.13; COBOL truncation is toward zero)
        assertThat(interest("-100.00", "1.55")).isEqualByComparingTo("-0.12");
    }

    @Test
    void matchesGoldenSampleValues() {
        // acct 1 after POSTTRAN: tcatbal(01,1) = 1164.87 at DEFAULT rate 15.00
        // (discgrp.txt DEFAULT/01/0001 = 0150{ → 15.00) → golden 14.56
        assertThat(interest("1164.87", "15.00")).isEqualByComparingTo("14.56");
    }

    @Test
    void zeroBalanceNonzeroRateYieldsZeroButStillCountsAsATransaction() {
        // 1300-B is unconditional inside 1300: a 0.00 interest txn IS written
        // (asserted end-to-end by BatchParityTest's 50-row interest golden,
        // which contains one zero-amount row); here we just pin the arithmetic
        assertThat(interest("0.00", "15.00")).isEqualByComparingTo("0.00");
    }
}
