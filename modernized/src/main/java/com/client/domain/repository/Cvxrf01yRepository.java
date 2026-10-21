package com.client.domain.repository;

import com.client.domain.model.Cvxrf01y;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CVXRF01Y
 * @citation cpy/CVXRF01Y.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Cvxrf01yRepository extends JpaRepository<Cvxrf01y, String> {
}
