package com.client.core.repository;

import com.client.core.model.Cobil00;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: COBIL00
 * @citation app/bms/COBIL00.bms
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Cobil00Repository extends JpaRepository<Cobil00, String> {
}
