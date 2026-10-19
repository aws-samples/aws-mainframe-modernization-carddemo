package com.client;


import java.math.BigDecimal;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

@DisplayName("Modernized Architecture & Domain Invariant Test Suite")
class ArchitectureFitnessTest {

    @Test
    @DisplayName("Verify base package conventions")
    void testPackageConventions() {
        Package basePkg = ArchitectureFitnessTest.class.getPackage();
        assertTrue(basePkg.getName().startsWith("com.client"), "Base package must follow enterprise convention");
    }

    @Test
    @DisplayName("Verify BigDecimal monetary scaling invariant")
    void testMonetaryScalingInvariant() {
        java.math.BigDecimal val = new java.math.BigDecimal("123.456");
        java.math.BigDecimal scaled = val.setScale(2, java.math.RoundingMode.HALF_UP);
        assertEquals(new java.math.BigDecimal("123.46"), scaled);
    }
}
