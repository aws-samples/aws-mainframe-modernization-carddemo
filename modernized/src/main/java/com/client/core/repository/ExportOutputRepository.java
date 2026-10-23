package com.client.core.repository;

import com.client.core.model.ExportOutput;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: EXPORT_OUTPUT
 * @citation EXPORT_OUTPUT (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface ExportOutputRepository extends JpaRepository<ExportOutput, String> {
}
