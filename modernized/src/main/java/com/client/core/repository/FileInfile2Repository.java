package com.client.core.repository;

import com.client.core.model.FileInfile2;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: FILE_INFILE2
 * @citation FILE_INFILE2 (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface FileInfile2Repository extends JpaRepository<FileInfile2, String> {
}
