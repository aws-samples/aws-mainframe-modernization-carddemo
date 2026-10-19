package com.client.core.repository;

import com.client.core.model.Cosgn00;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: COSGN00
 * @citation app/bms/COSGN00.bms
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Cosgn00Repository extends JpaRepository<Cosgn00, String> {
}
