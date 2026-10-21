package com.client.core.repository;

import com.client.core.model.Opfile1;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: OPFILE1
 * @citation OPFILE1 (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Opfile1Repository extends JpaRepository<Opfile1, String> {
}
