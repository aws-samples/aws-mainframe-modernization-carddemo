package com.client.core.repository;

import com.client.core.model.TrantypeFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: TRANTYPE_FILE
 * @citation TRANTYPE_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface TrantypeFileRepository extends JpaRepository<TrantypeFile, String> {
}
