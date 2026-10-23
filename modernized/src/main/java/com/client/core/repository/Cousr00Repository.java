package com.client.core.repository;

import com.client.core.model.Cousr00;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: COUSR00
 * @citation app/bms/COUSR00.bms
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Cousr00Repository extends JpaRepository<Cousr00, String> {
}
