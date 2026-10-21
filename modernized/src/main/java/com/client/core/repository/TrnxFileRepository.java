package com.client.core.repository;

import com.client.core.model.TrnxFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: TRNX_FILE
 * @citation TRNX_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface TrnxFileRepository extends JpaRepository<TrnxFile, String> {
}
