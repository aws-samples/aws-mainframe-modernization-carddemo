package com.client.core.repository;

import com.client.core.model.Cslkpcdy;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CSLKPCDY
 * @citation app/cpy/CSLKPCDY.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CslkpcdyRepository extends JpaRepository<Cslkpcdy, String> {
}
