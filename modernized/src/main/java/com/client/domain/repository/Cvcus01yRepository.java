package com.client.domain.repository;

import com.client.domain.model.Cvcus01y;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CVCUS01Y
 * @citation cpy/CVCUS01Y.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Cvcus01yRepository extends JpaRepository<Cvcus01y, Integer> {
}
