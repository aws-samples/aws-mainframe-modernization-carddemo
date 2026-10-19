package com.client.core.repository;

import com.client.core.model.Corpt00;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CORPT00
 * @citation app/bms/CORPT00.bms
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Corpt00Repository extends JpaRepository<Corpt00, String> {
}
