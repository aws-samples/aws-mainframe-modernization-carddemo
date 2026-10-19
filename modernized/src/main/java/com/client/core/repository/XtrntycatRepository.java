package com.client.core.repository;

import com.client.core.model.Xtrntycat;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: XTRNTYCAT
 * @citation app/app-transaction-type-db2/ddl/XTRNTYCAT.ddl
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface XtrntycatRepository extends JpaRepository<Xtrntycat, String> {
}
