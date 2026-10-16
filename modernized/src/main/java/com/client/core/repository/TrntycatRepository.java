package com.client.core.repository;

import com.client.core.model.Trntycat;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: TRNTYCAT
 * @citation app/app-transaction-type-db2/ddl/TRNTYCAT.ddl
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface TrntycatRepository extends JpaRepository<Trntycat, String> {
}
