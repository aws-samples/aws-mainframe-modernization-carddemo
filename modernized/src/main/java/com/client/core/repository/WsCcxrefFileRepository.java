package com.client.core.repository;

import com.client.core.model.WsCcxrefFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: WS-CCXREF-FILE
 * @citation WS-CCXREF-FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface WsCcxrefFileRepository extends JpaRepository<WsCcxrefFile, String> {
}
