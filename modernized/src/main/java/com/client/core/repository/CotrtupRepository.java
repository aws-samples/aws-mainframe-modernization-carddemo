package com.client.core.repository;

import com.client.core.model.Cotrtup;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: COTRTUP
 * @citation app/app-transaction-type-db2/bms/COTRTUP.bms
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CotrtupRepository extends JpaRepository<Cotrtup, String> {
}
