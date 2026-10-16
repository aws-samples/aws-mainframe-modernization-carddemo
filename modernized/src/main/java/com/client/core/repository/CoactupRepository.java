package com.client.core.repository;

import com.client.core.model.Coactup;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: COACTUP
 * @citation app/bms/COACTUP.bms
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CoactupRepository extends JpaRepository<Coactup, String> {
}
