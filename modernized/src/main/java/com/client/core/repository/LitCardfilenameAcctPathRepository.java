package com.client.core.repository;

import com.client.core.model.LitCardfilenameAcctPath;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: LIT-CARDFILENAME-ACCT-PATH
 * @citation LIT-CARDFILENAME-ACCT-PATH (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface LitCardfilenameAcctPathRepository extends JpaRepository<LitCardfilenameAcctPath, Long> {
}
