package com.client.core.repository;

import com.client.core.model.FileTransactionInput;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: FILE_TRANSACTION_INPUT
 * @citation FILE_TRANSACTION_INPUT (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface FileTransactionInputRepository extends JpaRepository<FileTransactionInput, String> {
}
