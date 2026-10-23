package com.client.core.repository;

import com.client.core.model.Transact;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: TRANSACT
 * @citation TRANSACT (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface TransactRepository extends JpaRepository<Transact, String> {
}
