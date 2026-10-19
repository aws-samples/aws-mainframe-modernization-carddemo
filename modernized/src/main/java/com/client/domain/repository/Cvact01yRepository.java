package com.client.domain.repository;

import com.client.domain.model.Cvact01y;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CVACT01Y
 * @citation cpy/CVACT01Y.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Cvact01yRepository extends JpaRepository<Cvact01y, Long> {
}
