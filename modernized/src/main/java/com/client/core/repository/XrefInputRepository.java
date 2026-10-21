package com.client.core.repository;

import com.client.core.model.XrefInput;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: XREF_INPUT
 * @citation XREF_INPUT (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface XrefInputRepository extends JpaRepository<XrefInput, String> {
}
