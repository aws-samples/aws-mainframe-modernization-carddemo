package com.client.core.repository;

import com.client.core.model.Cvtra05y;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CVTRA05Y
 * @citation app/cpy/CVTRA05Y.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Cvtra05yRepository extends JpaRepository<Cvtra05y, String> {
}
