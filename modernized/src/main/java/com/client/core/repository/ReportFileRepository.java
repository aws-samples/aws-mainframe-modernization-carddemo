package com.client.core.repository;

import com.client.core.model.ReportFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: REPORT_FILE
 * @citation REPORT_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface ReportFileRepository extends JpaRepository<ReportFile, String> {
}
