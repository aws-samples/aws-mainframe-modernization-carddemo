package com.client.core.repository;

import com.client.core.model.ArryFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: ARRY_FILE
 * @citation ARRY_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface ArryFileRepository extends JpaRepository<ArryFile, String> {
}
