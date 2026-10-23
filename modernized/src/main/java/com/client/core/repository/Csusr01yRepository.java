package com.client.core.repository;

import com.client.core.model.Csusr01y;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CSUSR01Y
 * @citation app/cpy/CSUSR01Y.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Csusr01yRepository extends JpaRepository<Csusr01y, String> {
}
