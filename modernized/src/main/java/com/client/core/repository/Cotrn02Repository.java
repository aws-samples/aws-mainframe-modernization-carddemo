package com.client.core.repository;

import com.client.core.model.Cotrn02;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: COTRN02
 * @citation app/bms/COTRN02.bms
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Cotrn02Repository extends JpaRepository<Cotrn02, String> {
}
