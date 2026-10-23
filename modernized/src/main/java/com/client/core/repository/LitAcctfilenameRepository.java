package com.client.core.repository;

import com.client.core.model.LitAcctfilename;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: LIT-ACCTFILENAME
 * @citation LIT-ACCTFILENAME (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface LitAcctfilenameRepository extends JpaRepository<LitAcctfilename, Long> {
}
