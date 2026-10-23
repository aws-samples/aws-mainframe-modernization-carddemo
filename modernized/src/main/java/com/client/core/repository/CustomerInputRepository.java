package com.client.core.repository;

import com.client.core.model.CustomerInput;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CUSTOMER_INPUT
 * @citation CUSTOMER_INPUT (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CustomerInputRepository extends JpaRepository<CustomerInput, Integer> {
}
