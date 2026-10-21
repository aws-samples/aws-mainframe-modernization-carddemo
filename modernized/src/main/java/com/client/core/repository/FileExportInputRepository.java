package com.client.core.repository;

import com.client.core.model.FileExportInput;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: FILE_EXPORT_INPUT
 * @citation FILE_EXPORT_INPUT (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface FileExportInputRepository extends JpaRepository<FileExportInput, String> {
}
