package com.client.core.repository;

import com.client.core.model.Coactvw;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: COACTVW
 * @citation app/bms/COACTVW.bms
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CoactvwRepository extends JpaRepository<Coactvw, String> {
}
