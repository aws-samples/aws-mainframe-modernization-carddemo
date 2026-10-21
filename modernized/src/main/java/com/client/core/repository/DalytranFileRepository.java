package com.client.core.repository;

import com.client.core.model.DalytranFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: DALYTRAN_FILE
 * @citation DALYTRAN_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface DalytranFileRepository extends JpaRepository<DalytranFile, String> {
}
