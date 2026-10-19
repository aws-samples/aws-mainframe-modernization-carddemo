package com.client.core.repository;

import com.client.core.model.Cvtra04y;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CVTRA04Y
 * @citation app/cpy/CVTRA04Y.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Cvtra04yRepository extends JpaRepository<Cvtra04y, String> {
}
