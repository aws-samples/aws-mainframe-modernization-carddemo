package com.client.core.repository;

import com.client.core.model.TransactionType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: TRANSACTION_TYPE
 * @citation TRANSACTION_TYPE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface TransactionTypeRepository extends JpaRepository<TransactionType, String> {
}
