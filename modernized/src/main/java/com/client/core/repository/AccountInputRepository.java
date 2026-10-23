package com.client.core.repository;

import com.client.core.model.AccountInput;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: ACCOUNT_INPUT
 * @citation ACCOUNT_INPUT (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface AccountInputRepository extends JpaRepository<AccountInput, Long> {
}
