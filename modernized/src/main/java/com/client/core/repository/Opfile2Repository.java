package com.client.core.repository;

import com.client.core.model.Opfile2;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: OPFILE2
 * @citation OPFILE2 (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Opfile2Repository extends JpaRepository<Opfile2, String> {
}
