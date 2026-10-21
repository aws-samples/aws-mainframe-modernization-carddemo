package com.client.core.repository;

import com.client.core.model.TransactionInput;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: TRANSACTION_INPUT
 * @citation TRANSACTION_INPUT (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface TransactionInputRepository extends JpaRepository<TransactionInput, String> {
}
