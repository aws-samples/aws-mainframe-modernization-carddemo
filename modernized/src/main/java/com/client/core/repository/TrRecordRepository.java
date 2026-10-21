package com.client.core.repository;

import com.client.core.model.TrRecord;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: TR_RECORD
 * @citation TR_RECORD (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface TrRecordRepository extends JpaRepository<TrRecord, String> {
}
