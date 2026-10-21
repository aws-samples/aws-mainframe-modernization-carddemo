package com.client.core.repository;

import com.client.core.model.Infile2;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: INFILE2
 * @citation INFILE2 (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Infile2Repository extends JpaRepository<Infile2, String> {
}
