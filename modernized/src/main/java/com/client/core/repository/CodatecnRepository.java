package com.client.core.repository;

import com.client.core.model.Codatecn;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CODATECN
 * @citation app/cpy/CODATECN.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CodatecnRepository extends JpaRepository<Codatecn, String> {
}
