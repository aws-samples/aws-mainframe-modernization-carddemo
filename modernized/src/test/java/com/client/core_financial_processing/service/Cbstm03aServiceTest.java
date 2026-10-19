package com.client.core_financial_processing.service;

import com.client.runtime.CobolState;
import com.client.runtime.ProgramService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("Cbstm03aService Deterministic Service Tests")
class Cbstm03aServiceTest {

    private Cbstm03aService service;

    @BeforeEach
    void setUp() {
        service = new Cbstm03aService();
    }

    @Test
    @DisplayName("Verify program metadata and source traceability")
    void testModuleMetadata() {
        assertEquals("CBSTM03A", service.programId());
        assertEquals("app/cbl/CBSTM03A.CBL", service.sourcePath());
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
