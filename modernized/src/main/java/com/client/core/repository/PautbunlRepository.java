package com.client.core.repository;

import com.client.core.model.Pautbunl;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: PAUTBUNL
 * @citation PAUTBUNL (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface PautbunlRepository extends JpaRepository<Pautbunl, String> {
}
