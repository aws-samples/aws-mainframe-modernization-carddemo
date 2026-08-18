package com.carddemo.etl;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class ZonedDecimalTest {

    @Test
    void positiveZeroOverpunch() {
        // ACCT-CURR-BAL from acctdata.txt line 1: PIC S9(10)V99 '00000001940{'
        // -> digits 000000019400, implied V99 -> +194.00
        assertThat(ZonedDecimal.parse("00000001940{", 2))
                .isEqualByComparingTo(new BigDecimal("194.00"));
    }

    @ParameterizedTest
    @CsvSource({
            // sign byte A-I => +1..+9
            "0000005047G, 2, 504.77",     // dailytran.txt line 1 amount (PIC S9(09)V99)
            "0000000001A, 2, 0.11",
            "0000000001I, 2, 0.19",
            // '{' => +0
            "00150{, 2, 15.00",           // DIS-INT-RATE PIC S9(04)V99 from discgrp.txt
            // plain trailing digit => positive
            "00000001234, 2, 12.34",
    })
    void positiveValues(String raw, int scale, String expected) {
        assertThat(ZonedDecimal.parse(raw, scale)).isEqualByComparingTo(new BigDecimal(expected));
    }

    @ParameterizedTest
    @CsvSource({
            // '}' => -0
            "0000009190}, 2, -919.00",    // dailytran.txt line 2 amount
            "00000{, 2, 0.00",
            // J-R => -1..-9
            "0000000001J, 2, -0.11",
            "0000000001R, 2, -0.19",
            "0000012345M, 2, -1234.54",
    })
    void negativeValues(String raw, int scale, String expected) {
        assertThat(ZonedDecimal.parse(raw, scale)).isEqualByComparingTo(new BigDecimal(expected));
    }

    @Test
    void negativeZeroEqualsZero() {
        assertThat(ZonedDecimal.parse("00000000000}", 2)).isEqualByComparingTo(BigDecimal.ZERO);
    }

    @Test
    void scaleZeroSupported() {
        assertThat(ZonedDecimal.parse("12C", 0)).isEqualByComparingTo(new BigDecimal("123"));
    }

    @Test
    void rejectsInvalidSignByte() {
        assertThatThrownBy(() -> ZonedDecimal.parse("0000000000Z", 2))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("sign byte");
    }

    @Test
    void rejectsNonDigitInBody() {
        assertThatThrownBy(() -> ZonedDecimal.parse("00X0000000{", 2))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("digit");
    }

    @Test
    void rejectsEmpty() {
        assertThatThrownBy(() -> ZonedDecimal.parse("", 2))
                .isInstanceOf(IllegalArgumentException.class);
    }
}
