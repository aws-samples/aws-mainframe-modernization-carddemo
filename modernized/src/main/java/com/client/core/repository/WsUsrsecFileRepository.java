package com.client.core.repository;

import com.client.core.model.WsUsrsecFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: WS-USRSEC-FILE
 * @citation WS-USRSEC-FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface WsUsrsecFileRepository extends JpaRepository<WsUsrsecFile, String> {
}
