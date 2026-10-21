package com.client.core.repository;

import com.client.core.model.FileVbrcFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: FILE_VBRC_FILE
 * @citation FILE_VBRC_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface FileVbrcFileRepository extends JpaRepository<FileVbrcFile, String> {
}
