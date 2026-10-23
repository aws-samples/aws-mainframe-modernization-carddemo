package com.client.core.repository;

import com.client.core.model.FileErrorOutput;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: FILE_ERROR_OUTPUT
 * @citation FILE_ERROR_OUTPUT (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface FileErrorOutputRepository extends JpaRepository<FileErrorOutput, String> {
}
