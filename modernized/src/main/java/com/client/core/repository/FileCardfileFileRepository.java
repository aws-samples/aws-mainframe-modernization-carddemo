package com.client.core.repository;

import com.client.core.model.FileCardfileFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: FILE_CARDFILE_FILE
 * @citation FILE_CARDFILE_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface FileCardfileFileRepository extends JpaRepository<FileCardfileFile, String> {
}
