package com.client.core.repository;

import com.client.core.model.FileArryFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: FILE_ARRY_FILE
 * @citation FILE_ARRY_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface FileArryFileRepository extends JpaRepository<FileArryFile, String> {
}
