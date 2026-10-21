package com.client.core_financial_processing.service;

import com.client.runtime.CobolState;
import com.client.runtime.ProgramService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("Cbact02cService Deterministic Service Tests")
class Cbact02cServiceTest {

    private Cbact02cService service;

    @BeforeEach
    void setUp() {
        service = new Cbact02cService();
    }

    @Test
    @DisplayName("Verify program metadata and source traceability")
    void testModuleMetadata() {
        assertEquals("CBACT02C", service.programId());
        assertEquals("app/cbl/CBACT02C.cbl", service.sourcePath());
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
