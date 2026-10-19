package com.client.core.repository;

import com.client.core.model.Cousr01;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: COUSR01
 * @citation app/bms/COUSR01.bms
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Cousr01Repository extends JpaRepository<Cousr01, String> {
}
