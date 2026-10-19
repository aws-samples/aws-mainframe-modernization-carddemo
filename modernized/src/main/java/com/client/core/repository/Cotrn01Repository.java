package com.client.core.repository;

import com.client.core.model.Cotrn01;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: COTRN01
 * @citation app/bms/COTRN01.bms
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Cotrn01Repository extends JpaRepository<Cotrn01, String> {
}
