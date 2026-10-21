package com.client.core.repository;

import com.client.core.model.FileAccountOutput;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: FILE_ACCOUNT_OUTPUT
 * @citation FILE_ACCOUNT_OUTPUT (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface FileAccountOutputRepository extends JpaRepository<FileAccountOutput, Long> {
}
