package com.client.core.repository;

import com.client.core.model.AcctFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: ACCT_FILE
 * @citation ACCT_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface AcctFileRepository extends JpaRepository<AcctFile, Long> {
}
