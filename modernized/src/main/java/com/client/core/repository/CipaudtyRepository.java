package com.client.core.repository;

import com.client.core.model.Cipaudty;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CIPAUDTY
 * @citation app/app-authorization-ims-db2-mq/cpy/CIPAUDTY.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CipaudtyRepository extends JpaRepository<Cipaudty, String> {
}
