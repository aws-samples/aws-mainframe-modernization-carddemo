package com.client.core.repository;

import com.client.core.model.AcctfileFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: ACCTFILE_FILE
 * @citation ACCTFILE_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface AcctfileFileRepository extends JpaRepository<AcctfileFile, Long> {
}
