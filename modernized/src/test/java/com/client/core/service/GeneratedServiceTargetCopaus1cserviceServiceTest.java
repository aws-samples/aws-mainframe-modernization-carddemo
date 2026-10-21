package com.client.core.service;

import com.client.runtime.CobolState;
import com.client.runtime.ProgramService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("GeneratedServiceTargetCopaus1cserviceService Deterministic Service Tests")
class GeneratedServiceTargetCopaus1cserviceServiceTest {

    private GeneratedServiceTargetCopaus1cserviceService service;

    @BeforeEach
    void setUp() {
        service = new GeneratedServiceTargetCopaus1cserviceService();
    }

    @Test
    @DisplayName("Verify program metadata and source traceability")
    void testModuleMetadata() {
        assertEquals("generated_service:TARGET_COPAUS1CSERVICE", service.programId());
        assertEquals("generated_service:TARGET_COPAUS1CSERVICE (source not found)", service.sourcePath());
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
