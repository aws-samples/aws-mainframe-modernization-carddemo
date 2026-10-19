package com.client.core.repository;

import com.client.core.model.Padfldbd;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: PADFLDBD
 * @citation PADFLDBD (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface PadfldbdRepository extends JpaRepository<Padfldbd, String> {
}
