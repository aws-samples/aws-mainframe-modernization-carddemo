package com.client.core.repository;

import com.client.core.model.TransactFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: TRANSACT_FILE
 * @citation TRANSACT_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface TransactFileRepository extends JpaRepository<TransactFile, String> {
}
