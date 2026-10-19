package com.client.core.repository;

import com.client.core.model.Cvtra07y;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CVTRA07Y
 * @citation app/cpy/CVTRA07Y.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Cvtra07yRepository extends JpaRepository<Cvtra07y, String> {
}
