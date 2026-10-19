package com.client.domain.repository;

import com.client.domain.model.Cvtra01y;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CVTRA01Y
 * @citation cpy/CVTRA01Y.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Cvtra01yRepository extends JpaRepository<Cvtra01y, String> {
}
