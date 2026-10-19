package com.client.core.repository;

import com.client.core.model.Dligsamp;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: DLIGSAMP
 * @citation DLIGSAMP (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface DligsampRepository extends JpaRepository<Dligsamp, String> {
}
