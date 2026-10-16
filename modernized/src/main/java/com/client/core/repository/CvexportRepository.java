package com.client.core.repository;

import com.client.core.model.Cvexport;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CVEXPORT
 * @citation app/cpy/CVEXPORT.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CvexportRepository extends JpaRepository<Cvexport, String> {
}
