package com.client.core.repository;

import com.client.core.model.TransactionTypeCategory;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: TRANSACTION_TYPE_CATEGORY
 * @citation TRANSACTION_TYPE_CATEGORY (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface TransactionTypeCategoryRepository extends JpaRepository<TransactionTypeCategory, String> {
}
