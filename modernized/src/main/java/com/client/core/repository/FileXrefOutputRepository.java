package com.client.core.repository;

import com.client.core.model.FileXrefOutput;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: FILE_XREF_OUTPUT
 * @citation FILE_XREF_OUTPUT (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface FileXrefOutputRepository extends JpaRepository<FileXrefOutput, String> {
}
