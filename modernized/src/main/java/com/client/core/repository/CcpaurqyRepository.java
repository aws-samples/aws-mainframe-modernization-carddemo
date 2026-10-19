package com.client.core.repository;

import com.client.core.model.Ccpaurqy;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CCPAURQY
 * @citation app/app-authorization-ims-db2-mq/cpy/CCPAURQY.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CcpaurqyRepository extends JpaRepository<Ccpaurqy, String> {
}
