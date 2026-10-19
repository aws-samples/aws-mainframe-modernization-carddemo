package com.client.core.repository;

import com.client.core.model.Comen01;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: COMEN01
 * @citation app/bms/COMEN01.bms
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Comen01Repository extends JpaRepository<Comen01, String> {
}
