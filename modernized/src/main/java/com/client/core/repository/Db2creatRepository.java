package com.client.core.repository;

import com.client.core.model.Db2creat;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: DB2CREAT
 * @citation DB2CREAT (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Db2creatRepository extends JpaRepository<Db2creat, String> {
}
