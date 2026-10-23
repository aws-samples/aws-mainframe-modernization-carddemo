package com.client.core.repository;

import com.client.core.model.DalyrejsFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: DALYREJS_FILE
 * @citation DALYREJS_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface DalyrejsFileRepository extends JpaRepository<DalyrejsFile, String> {
}
