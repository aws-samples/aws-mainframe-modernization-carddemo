package com.client.core.repository;

import com.client.core.model.ErrorOutput;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: ERROR_OUTPUT
 * @citation ERROR_OUTPUT (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface ErrorOutputRepository extends JpaRepository<ErrorOutput, String> {
}
