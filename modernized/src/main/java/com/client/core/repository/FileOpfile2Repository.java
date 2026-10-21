package com.client.core.repository;

import com.client.core.model.FileOpfile2;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: FILE_OPFILE2
 * @citation FILE_OPFILE2 (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface FileOpfile2Repository extends JpaRepository<FileOpfile2, String> {
}
