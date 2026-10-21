package com.client.core.service;

import com.client.runtime.CobolState;
import com.client.runtime.ProgramService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("IacGoogleCloudRunV2ServiceTransactionServiceService Deterministic Service Tests")
class IacGoogleCloudRunV2ServiceTransactionServiceServiceTest {

    private IacGoogleCloudRunV2ServiceTransactionServiceService service;

    @BeforeEach
    void setUp() {
        service = new IacGoogleCloudRunV2ServiceTransactionServiceService();
    }

    @Test
    @DisplayName("Verify program metadata and source traceability")
    void testModuleMetadata() {
        assertEquals("iac_google_cloud_run_v2_service_transaction_service", service.programId());
        assertEquals("iac_google_cloud_run_v2_service_transaction_service (source not found)", service.sourcePath());
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
