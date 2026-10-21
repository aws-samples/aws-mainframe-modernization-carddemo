package com.client.core.repository;

import com.client.core.model.XrefFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: XREF_FILE
 * @citation XREF_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface XrefFileRepository extends JpaRepository<XrefFile, String> {
}
