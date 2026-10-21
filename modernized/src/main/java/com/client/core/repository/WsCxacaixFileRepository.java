package com.client.core.repository;

import com.client.core.model.WsCxacaixFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: WS-CXACAIX-FILE
 * @citation WS-CXACAIX-FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface WsCxacaixFileRepository extends JpaRepository<WsCxacaixFile, String> {
}
