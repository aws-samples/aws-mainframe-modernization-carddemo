package com.client.core.repository;

import com.client.core.model.XreffileFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: XREFFILE_FILE
 * @citation XREFFILE_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface XreffileFileRepository extends JpaRepository<XreffileFile, String> {
}
