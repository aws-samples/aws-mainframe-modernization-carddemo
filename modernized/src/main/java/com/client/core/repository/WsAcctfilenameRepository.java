package com.client.core.repository;

import com.client.core.model.WsAcctfilename;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: WS-ACCTFILENAME
 * @citation WS-ACCTFILENAME (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface WsAcctfilenameRepository extends JpaRepository<WsAcctfilename, Long> {
}
