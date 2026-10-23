package com.client.core.repository;

import com.client.core.model.FileStmtFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: FILE_STMT_FILE
 * @citation FILE_STMT_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface FileStmtFileRepository extends JpaRepository<FileStmtFile, String> {
}
