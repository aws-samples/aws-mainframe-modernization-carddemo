package com.client.core.repository;

import com.client.core.model.Comen02y;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: COMEN02Y
 * @citation app/cpy/COMEN02Y.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Comen02yRepository extends JpaRepository<Comen02y, String> {
}
