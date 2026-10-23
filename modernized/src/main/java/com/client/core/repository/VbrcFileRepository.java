package com.client.core.repository;

import com.client.core.model.VbrcFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: VBRC_FILE
 * @citation VBRC_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface VbrcFileRepository extends JpaRepository<VbrcFile, String> {
}
