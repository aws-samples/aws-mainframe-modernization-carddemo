package com.client.core.repository;

import com.client.core.model.Pasflpcb;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: PASFLPCB
 * @citation app/app-authorization-ims-db2-mq/cpy/PASFLPCB.CPY
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface PasflpcbRepository extends JpaRepository<Pasflpcb, String> {
}
