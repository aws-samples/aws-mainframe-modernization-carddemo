package com.client.core.repository;

import com.client.core.model.Cocrdup;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: COCRDUP
 * @citation app/bms/COCRDUP.bms
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CocrdupRepository extends JpaRepository<Cocrdup, String> {
}
