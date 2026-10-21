package com.client.core.repository;

import com.client.core.model.Cotrn00;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: COTRN00
 * @citation app/bms/COTRN00.bms
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Cotrn00Repository extends JpaRepository<Cotrn00, String> {
}
