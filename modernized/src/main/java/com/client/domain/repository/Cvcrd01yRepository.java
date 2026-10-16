package com.client.domain.repository;

import com.client.domain.model.Cvcrd01y;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CVCRD01Y
 * @citation cpy/CVCRD01Y.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Cvcrd01yRepository extends JpaRepository<Cvcrd01y, String> {
}
