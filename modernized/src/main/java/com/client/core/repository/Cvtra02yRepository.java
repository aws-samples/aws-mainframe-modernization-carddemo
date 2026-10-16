package com.client.core.repository;

import com.client.core.model.Cvtra02y;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CVTRA02Y
 * @citation app/cpy/CVTRA02Y.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Cvtra02yRepository extends JpaRepository<Cvtra02y, String> {
}
