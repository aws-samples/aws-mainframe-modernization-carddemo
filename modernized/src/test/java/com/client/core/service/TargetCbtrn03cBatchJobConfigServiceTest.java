package com.client.core.service;

import com.client.runtime.CobolState;
import com.client.runtime.ProgramService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("TargetCbtrn03cBatchJobConfigService Deterministic Service Tests")
class TargetCbtrn03cBatchJobConfigServiceTest {

    private TargetCbtrn03cBatchJobConfigService service;

    @BeforeEach
    void setUp() {
        service = new TargetCbtrn03cBatchJobConfigService();
    }

    @Test
    @DisplayName("Verify program metadata and source traceability")
    void testModuleMetadata() {
        assertEquals("target_Cbtrn03cBatchJobConfig", service.programId());
        assertEquals("target_Cbtrn03cBatchJobConfig (source not found)", service.sourcePath());
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
