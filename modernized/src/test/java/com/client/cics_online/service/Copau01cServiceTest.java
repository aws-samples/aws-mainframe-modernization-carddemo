package com.client.cics_online.service;

import com.client.runtime.CobolState;
import com.client.runtime.ProgramService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("Copau01cService Deterministic Service Tests")
class Copau01cServiceTest {

    private Copau01cService service;

    @BeforeEach
    void setUp() {
        service = new Copau01cService();
    }

    @Test
    @DisplayName("Verify program metadata and source traceability")
    void testModuleMetadata() {
        assertEquals("COPAU01C", service.programId());
        assertEquals("app/cbl/COPAU01C.cbl", service.sourcePath());
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
