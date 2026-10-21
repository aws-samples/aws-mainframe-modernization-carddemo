package com.client.core.repository;

import com.client.core.model.Cvtra03y;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CVTRA03Y
 * @citation app/cpy/CVTRA03Y.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Cvtra03yRepository extends JpaRepository<Cvtra03y, String> {
}
