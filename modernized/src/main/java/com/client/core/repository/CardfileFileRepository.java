package com.client.core.repository;

import com.client.core.model.CardfileFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CARDFILE_FILE
 * @citation CARDFILE_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CardfileFileRepository extends JpaRepository<CardfileFile, String> {
}
