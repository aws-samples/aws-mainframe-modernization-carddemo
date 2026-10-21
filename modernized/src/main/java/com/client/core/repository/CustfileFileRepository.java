package com.client.core.repository;

import com.client.core.model.CustfileFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CUSTFILE_FILE
 * @citation CUSTFILE_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CustfileFileRepository extends JpaRepository<CustfileFile, Integer> {
}
