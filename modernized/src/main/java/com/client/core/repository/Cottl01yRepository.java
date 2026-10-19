package com.client.core.repository;

import com.client.core.model.Cottl01y;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: COTTL01Y
 * @citation app/cpy/COTTL01Y.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Cottl01yRepository extends JpaRepository<Cottl01y, String> {
}
