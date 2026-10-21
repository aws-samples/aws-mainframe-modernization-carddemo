package com.client.core.repository;

import com.client.core.model.LitCardxrefnameAcctPath;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: LIT-CARDXREFNAME-ACCT-PATH
 * @citation LIT-CARDXREFNAME-ACCT-PATH (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface LitCardxrefnameAcctPathRepository extends JpaRepository<LitCardxrefnameAcctPath, Long> {
}
