package com.client.core.repository;

import com.client.core.model.Trnxfile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: TRNXFILE
 * @citation TRNXFILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface TrnxfileRepository extends JpaRepository<Trnxfile, String> {
}
