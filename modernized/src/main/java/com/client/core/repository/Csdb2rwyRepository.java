package com.client.core.repository;

import com.client.core.model.Csdb2rwy;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CSDB2RWY
 * @citation app/app-transaction-type-db2/cpy/CSDB2RWY.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Csdb2rwyRepository extends JpaRepository<Csdb2rwy, String> {
}
