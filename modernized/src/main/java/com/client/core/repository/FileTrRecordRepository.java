package com.client.core.repository;

import com.client.core.model.FileTrRecord;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: FILE_TR_RECORD
 * @citation FILE_TR_RECORD (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface FileTrRecordRepository extends JpaRepository<FileTrRecord, String> {
}
