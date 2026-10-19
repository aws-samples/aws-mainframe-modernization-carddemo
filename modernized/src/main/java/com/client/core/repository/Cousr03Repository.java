package com.client.core.repository;

import com.client.core.model.Cousr03;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: COUSR03
 * @citation app/bms/COUSR03.bms
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Cousr03Repository extends JpaRepository<Cousr03, String> {
}
