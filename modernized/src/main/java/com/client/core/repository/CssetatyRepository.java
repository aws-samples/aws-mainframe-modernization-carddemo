package com.client.core.repository;

import com.client.core.model.Cssetaty;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CSSETATY
 * @citation app/cpy/CSSETATY.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CssetatyRepository extends JpaRepository<Cssetaty, String> {
}
