package com.client.core.repository;

import com.client.core.model.CardOutput;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CARD_OUTPUT
 * @citation CARD_OUTPUT (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CardOutputRepository extends JpaRepository<CardOutput, String> {
}
