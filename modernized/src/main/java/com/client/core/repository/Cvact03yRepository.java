package com.client.core.repository;

import com.client.core.model.Cvact03y;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CVACT03Y
 * @citation app/cpy/CVACT03Y.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Cvact03yRepository extends JpaRepository<Cvact03y, Long> {
}
