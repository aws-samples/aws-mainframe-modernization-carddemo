package com.client.core.repository;

import com.client.core.model.LitCardfilename;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: LIT-CARDFILENAME
 * @citation LIT-CARDFILENAME (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface LitCardfilenameRepository extends JpaRepository<LitCardfilename, String> {
}
