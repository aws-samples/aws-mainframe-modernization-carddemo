package com.client.core.repository;

import com.client.core.model.Csutldpy;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CSUTLDPY
 * @citation app/cpy/CSUTLDPY.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CsutldpyRepository extends JpaRepository<Csutldpy, String> {
}
