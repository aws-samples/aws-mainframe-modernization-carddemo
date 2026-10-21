package com.client.core.repository;

import com.client.core.model.WsAcctdatFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: WS-ACCTDAT-FILE
 * @citation WS-ACCTDAT-FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface WsAcctdatFileRepository extends JpaRepository<WsAcctdatFile, Long> {
}
