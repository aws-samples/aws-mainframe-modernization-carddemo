package com.client.core.repository;

import com.client.core.model.CustomerFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CUSTOMER_FILE
 * @citation CUSTOMER_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CustomerFileRepository extends JpaRepository<CustomerFile, Integer> {
}
