package com.client.core.repository;

import com.client.core.model.CustomerOutput;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CUSTOMER_OUTPUT
 * @citation CUSTOMER_OUTPUT (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CustomerOutputRepository extends JpaRepository<CustomerOutput, Integer> {
}
