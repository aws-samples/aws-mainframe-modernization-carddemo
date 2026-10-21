package com.client.core.repository;

import com.client.core.model.Csdat01y;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CSDAT01Y
 * @citation app/cpy/CSDAT01Y.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Csdat01yRepository extends JpaRepository<Csdat01y, String> {
}
