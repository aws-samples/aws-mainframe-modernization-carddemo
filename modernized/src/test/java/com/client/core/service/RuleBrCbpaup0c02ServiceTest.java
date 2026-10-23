package com.client.core.service;

import com.client.runtime.CobolState;
import com.client.runtime.ProgramService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("RuleBrCbpaup0c02Service Deterministic Service Tests")
class RuleBrCbpaup0c02ServiceTest {

    private RuleBrCbpaup0c02Service service;

    @BeforeEach
    void setUp() {
        service = new RuleBrCbpaup0c02Service();
    }

    @Test
    @DisplayName("Verify program metadata and source traceability")
    void testModuleMetadata() {
        assertEquals("rule_BR_CBPAUP0C_02", service.programId());
        assertEquals("rule_BR_CBPAUP0C_02 (source not found)", service.sourcePath());
        assertTrue(service instanceof ProgramService);
    }

    @Test
    @DisplayName("Verify CobolState initialization")
    void testNewStateInitialization() {
        CobolState state = service.newState();
        assertNotNull(state, "CobolState must initialize with field definitions");
    }

    @Test
    @DisplayName("Verify rules catalog accessibility")
    void testRulesCatalog() {
        assertNotNull(service.rules(), "Rules catalog must be populated");
    }
}
