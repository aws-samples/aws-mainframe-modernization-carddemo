package com.client.core.repository;

import com.client.core.model.WsCardxrefnameAcctPath;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: WS-CARDXREFNAME-ACCT-PATH
 * @citation WS-CARDXREFNAME-ACCT-PATH (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface WsCardxrefnameAcctPathRepository extends JpaRepository<WsCardxrefnameAcctPath, Long> {
}
