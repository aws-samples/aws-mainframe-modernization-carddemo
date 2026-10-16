package com.client.core.repository;

import com.client.core.model.Costm01;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: COSTM01
 * @citation app/cpy/COSTM01.CPY
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Costm01Repository extends JpaRepository<Costm01, String> {
}
