package com.client.core.repository;

import com.client.core.model.FileDateParmsFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: FILE_DATE_PARMS_FILE
 * @citation FILE_DATE_PARMS_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface FileDateParmsFileRepository extends JpaRepository<FileDateParmsFile, String> {
}
