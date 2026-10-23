package com.client.core.repository;

import com.client.core.model.FileExportOutput;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: FILE_EXPORT_OUTPUT
 * @citation FILE_EXPORT_OUTPUT (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface FileExportOutputRepository extends JpaRepository<FileExportOutput, String> {
}
