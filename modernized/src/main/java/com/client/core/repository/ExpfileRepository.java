package com.client.core.repository;

import com.client.core.model.Expfile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: EXPFILE
 * @citation EXPFILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface ExpfileRepository extends JpaRepository<Expfile, String> {
}
