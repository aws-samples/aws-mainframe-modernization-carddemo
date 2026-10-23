package com.client.core.repository;

import com.client.core.model.Infile1;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: INFILE1
 * @citation INFILE1 (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Infile1Repository extends JpaRepository<Infile1, String> {
}
