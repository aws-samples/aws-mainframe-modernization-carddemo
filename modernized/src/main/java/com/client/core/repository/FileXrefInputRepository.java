package com.client.core.repository;

import com.client.core.model.FileXrefInput;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: FILE_XREF_INPUT
 * @citation FILE_XREF_INPUT (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface FileXrefInputRepository extends JpaRepository<FileXrefInput, String> {
}
