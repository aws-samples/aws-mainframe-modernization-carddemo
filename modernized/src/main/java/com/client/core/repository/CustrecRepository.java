package com.client.core.repository;

import com.client.core.model.Custrec;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CUSTREC
 * @citation app/cpy/CUSTREC.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CustrecRepository extends JpaRepository<Custrec, Integer> {
}
