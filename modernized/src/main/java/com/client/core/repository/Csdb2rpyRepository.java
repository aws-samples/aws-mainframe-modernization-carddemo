package com.client.core.repository;

import com.client.core.model.Csdb2rpy;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CSDB2RPY
 * @citation app/app-transaction-type-db2/cpy/CSDB2RPY.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Csdb2rpyRepository extends JpaRepository<Csdb2rpy, String> {
}
