package com.client.core.repository;

import com.client.core.model.FileDalyrejsFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: FILE_DALYREJS_FILE
 * @citation FILE_DALYREJS_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface FileDalyrejsFileRepository extends JpaRepository<FileDalyrejsFile, String> {
}
