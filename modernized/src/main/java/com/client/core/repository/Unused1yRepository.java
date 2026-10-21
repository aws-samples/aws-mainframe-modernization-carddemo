package com.client.core.repository;

import com.client.core.model.Unused1y;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: UNUSED1Y
 * @citation app/cpy/UNUSED1Y.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Unused1yRepository extends JpaRepository<Unused1y, String> {
}
