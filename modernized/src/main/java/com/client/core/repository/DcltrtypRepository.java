package com.client.core.repository;

import com.client.core.model.Dcltrtyp;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: DCLTRTYP
 * @citation app/app-transaction-type-db2/dcl/DCLTRTYP.dcl
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface DcltrtypRepository extends JpaRepository<Dcltrtyp, String> {
}
