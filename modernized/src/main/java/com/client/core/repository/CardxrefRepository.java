package com.client.core.repository;

import com.client.core.model.Cardxref;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CARDXREF
 * @citation app/data/ASCII/cardxref.txt
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CardxrefRepository extends JpaRepository<Cardxref, String> {
}
