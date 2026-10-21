package com.client.core.repository;

import com.client.core.model.HtmlFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: HTML_FILE
 * @citation HTML_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface HtmlFileRepository extends JpaRepository<HtmlFile, String> {
}
