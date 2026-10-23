package com.client.core.repository;

import com.client.core.model.FileTcatbalFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: FILE_TCATBAL_FILE
 * @citation FILE_TCATBAL_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface FileTcatbalFileRepository extends JpaRepository<FileTcatbalFile, String> {
}
