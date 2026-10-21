package com.client.core_financial_processing.service;

import com.client.runtime.CobolState;
import com.client.runtime.ProgramService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("CobswaitService Deterministic Service Tests")
class CobswaitServiceTest {

    private CobswaitService service;

    @BeforeEach
    void setUp() {
        service = new CobswaitService();
    }

    @Test
    @DisplayName("Verify program metadata and source traceability")
    void testModuleMetadata() {
        assertEquals("COBSWAIT", service.programId());
        assertEquals("app/cbl/COBSWAIT.cbl", service.sourcePath());
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
