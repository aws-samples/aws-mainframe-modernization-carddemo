package com.client.core.repository;

import com.client.core.model.Padflpcb;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: PADFLPCB
 * @citation app/app-authorization-ims-db2-mq/cpy/PADFLPCB.CPY
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface PadflpcbRepository extends JpaRepository<Padflpcb, String> {
}
