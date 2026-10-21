package com.client.core.repository;

import com.client.core.model.LitCardFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: LIT-CARD-FILE
 * @citation LIT-CARD-FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface LitCardFileRepository extends JpaRepository<LitCardFile, String> {
}
