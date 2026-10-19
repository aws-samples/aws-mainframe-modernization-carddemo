package com.client.core.repository;

import com.client.core.model.Imsfuncs;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: IMSFUNCS
 * @citation app/app-authorization-ims-db2-mq/cpy/IMSFUNCS.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface ImsfuncsRepository extends JpaRepository<Imsfuncs, String> {
}
