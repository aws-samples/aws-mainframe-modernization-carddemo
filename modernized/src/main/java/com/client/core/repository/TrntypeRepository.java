package com.client.core.repository;

import com.client.core.model.Trntype;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: TRNTYPE
 * @citation app/app-transaction-type-db2/ddl/TRNTYPE.ddl
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface TrntypeRepository extends JpaRepository<Trntype, String> {
}
