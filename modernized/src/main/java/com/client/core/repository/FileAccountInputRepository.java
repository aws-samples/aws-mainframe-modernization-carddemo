package com.client.core.repository;

import com.client.core.model.FileAccountInput;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: FILE_ACCOUNT_INPUT
 * @citation FILE_ACCOUNT_INPUT (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface FileAccountInputRepository extends JpaRepository<FileAccountInput, Long> {
}
