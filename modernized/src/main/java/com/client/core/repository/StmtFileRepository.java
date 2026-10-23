package com.client.core.repository;

import com.client.core.model.StmtFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: STMT_FILE
 * @citation STMT_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface StmtFileRepository extends JpaRepository<StmtFile, String> {
}
