package com.client.core.repository;

import com.client.core.model.Csmsg02y;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CSMSG02Y
 * @citation app/cpy/CSMSG02Y.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Csmsg02yRepository extends JpaRepository<Csmsg02y, String> {
}
