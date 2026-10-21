package com.client.core.repository;

import com.client.core.model.Cotrtli;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: COTRTLI
 * @citation app/app-transaction-type-db2/bms/COTRTLI.bms
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CotrtliRepository extends JpaRepository<Cotrtli, String> {
}
