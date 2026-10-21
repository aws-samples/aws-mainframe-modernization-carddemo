package com.client.core.repository;

import com.client.core.model.CustFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CUST_FILE
 * @citation CUST_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CustFileRepository extends JpaRepository<CustFile, Integer> {
}
