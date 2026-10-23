package com.client.core.repository;

import com.client.core.model.XrefOutput;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: XREF_OUTPUT
 * @citation XREF_OUTPUT (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface XrefOutputRepository extends JpaRepository<XrefOutput, String> {
}
