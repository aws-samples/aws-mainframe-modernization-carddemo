package com.client.core.repository;

import com.client.core.model.Cvtra06y;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CVTRA06Y
 * @citation app/cpy/CVTRA06Y.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Cvtra06yRepository extends JpaRepository<Cvtra06y, String> {
}
