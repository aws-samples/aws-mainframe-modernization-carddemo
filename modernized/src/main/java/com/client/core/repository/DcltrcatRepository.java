package com.client.core.repository;

import com.client.core.model.Dcltrcat;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: DCLTRCAT
 * @citation app/app-transaction-type-db2/dcl/DCLTRCAT.dcl
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface DcltrcatRepository extends JpaRepository<Dcltrcat, String> {
}
