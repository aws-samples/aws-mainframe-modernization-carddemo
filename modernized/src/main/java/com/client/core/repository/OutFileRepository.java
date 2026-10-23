package com.client.core.repository;

import com.client.core.model.OutFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: OUT_FILE
 * @citation OUT_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface OutFileRepository extends JpaRepository<OutFile, String> {
}
