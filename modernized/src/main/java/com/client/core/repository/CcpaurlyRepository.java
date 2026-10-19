package com.client.core.repository;

import com.client.core.model.Ccpaurly;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CCPAURLY
 * @citation app/app-authorization-ims-db2-mq/cpy/CCPAURLY.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CcpaurlyRepository extends JpaRepository<Ccpaurly, String> {
}
