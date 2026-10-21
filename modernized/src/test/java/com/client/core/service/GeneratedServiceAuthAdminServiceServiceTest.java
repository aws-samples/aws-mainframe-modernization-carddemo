package com.client.core.service;

import com.client.runtime.CobolState;
import com.client.runtime.ProgramService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("GeneratedServiceAuthAdminServiceService Deterministic Service Tests")
class GeneratedServiceAuthAdminServiceServiceTest {

    private GeneratedServiceAuthAdminServiceService service;

    @BeforeEach
    void setUp() {
        service = new GeneratedServiceAuthAdminServiceService();
    }

    @Test
    @DisplayName("Verify program metadata and source traceability")
    void testModuleMetadata() {
        assertEquals("generated_service:AUTH-ADMIN-SERVICE", service.programId());
        assertEquals("generated_service:AUTH-ADMIN-SERVICE (source not found)", service.sourcePath());
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
