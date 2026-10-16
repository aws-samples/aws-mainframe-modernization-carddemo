package com.client.core.repository;

import com.client.core.model.Authfrds;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: AUTHFRDS
 * @citation app/app-authorization-ims-db2-mq/dcl/AUTHFRDS.dcl
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface AuthfrdsRepository extends JpaRepository<Authfrds, String> {
}
