package com.client.core.repository;

import com.client.core.model.Ccpauery;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CCPAUERY
 * @citation app/app-authorization-ims-db2-mq/cpy/CCPAUERY.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CcpaueryRepository extends JpaRepository<Ccpauery, String> {
}
