package com.client.core.repository;

import com.client.core.model.Csmsg01y;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CSMSG01Y
 * @citation app/cpy/CSMSG01Y.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Csmsg01yRepository extends JpaRepository<Csmsg01y, String> {
}
