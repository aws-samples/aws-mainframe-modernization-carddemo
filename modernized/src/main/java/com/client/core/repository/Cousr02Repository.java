package com.client.core.repository;

import com.client.core.model.Cousr02;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: COUSR02
 * @citation app/bms/COUSR02.bms
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Cousr02Repository extends JpaRepository<Cousr02, String> {
}
