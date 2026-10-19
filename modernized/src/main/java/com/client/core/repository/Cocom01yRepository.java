package com.client.core.repository;

import com.client.core.model.Cocom01y;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: COCOM01Y
 * @citation app/cpy/COCOM01Y.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Cocom01yRepository extends JpaRepository<Cocom01y, String> {
}
