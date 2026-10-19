package com.client.core.repository;

import com.client.core.model.Pasfldbd;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: PASFLDBD
 * @citation PASFLDBD (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface PasfldbdRepository extends JpaRepository<Pasfldbd, String> {
}
