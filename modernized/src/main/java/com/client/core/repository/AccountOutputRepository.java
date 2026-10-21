package com.client.core.repository;

import com.client.core.model.AccountOutput;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: ACCOUNT_OUTPUT
 * @citation ACCOUNT_OUTPUT (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface AccountOutputRepository extends JpaRepository<AccountOutput, Long> {
}
