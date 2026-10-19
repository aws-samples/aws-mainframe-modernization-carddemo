package com.client.core.repository;

import com.client.core.model.Copau00;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: COPAU00
 * @citation app/app-authorization-ims-db2-mq/bms/COPAU00.bms
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Copau00Repository extends JpaRepository<Copau00, String> {
}
