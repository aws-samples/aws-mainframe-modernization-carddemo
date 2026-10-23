package com.client.core.repository;

import com.client.core.model.TcatbalFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: TCATBAL_FILE
 * @citation TCATBAL_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface TcatbalFileRepository extends JpaRepository<TcatbalFile, String> {
}
