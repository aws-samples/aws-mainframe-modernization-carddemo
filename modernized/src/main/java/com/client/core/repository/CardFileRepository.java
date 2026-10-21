package com.client.core.repository;

import com.client.core.model.CardFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CARD_FILE
 * @citation CARD_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CardFileRepository extends JpaRepository<CardFile, String> {
}
