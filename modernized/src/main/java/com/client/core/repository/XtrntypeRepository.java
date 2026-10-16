package com.client.core.repository;

import com.client.core.model.Xtrntype;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: XTRNTYPE
 * @citation app/app-transaction-type-db2/ddl/XTRNTYPE.ddl
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface XtrntypeRepository extends JpaRepository<Xtrntype, String> {
}
