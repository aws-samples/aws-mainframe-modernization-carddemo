package com.client.core.repository;

import com.client.core.model.Copau01;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: COPAU01
 * @citation app/app-authorization-ims-db2-mq/bms/COPAU01.bms
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Copau01Repository extends JpaRepository<Copau01, String> {
}
