package com.client.core.repository;

import com.client.core.model.Xauthfrd;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: XAUTHFRD
 * @citation app/app-authorization-ims-db2-mq/ddl/XAUTHFRD.ddl
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface XauthfrdRepository extends JpaRepository<Xauthfrd, String> {
}
