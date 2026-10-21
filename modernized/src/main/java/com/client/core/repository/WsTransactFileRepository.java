package com.client.core.repository;

import com.client.core.model.WsTransactFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: WS-TRANSACT-FILE
 * @citation WS-TRANSACT-FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface WsTransactFileRepository extends JpaRepository<WsTransactFile, String> {
}
