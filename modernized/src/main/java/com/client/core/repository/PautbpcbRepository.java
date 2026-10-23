package com.client.core.repository;

import com.client.core.model.Pautbpcb;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: PAUTBPCB
 * @citation app/app-authorization-ims-db2-mq/cpy/PAUTBPCB.CPY
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface PautbpcbRepository extends JpaRepository<Pautbpcb, String> {
}
