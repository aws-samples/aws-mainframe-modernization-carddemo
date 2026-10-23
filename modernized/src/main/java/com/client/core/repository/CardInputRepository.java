package com.client.core.repository;

import com.client.core.model.CardInput;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CARD_INPUT
 * @citation CARD_INPUT (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CardInputRepository extends JpaRepository<CardInput, String> {
}
