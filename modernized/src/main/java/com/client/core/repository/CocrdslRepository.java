package com.client.core.repository;

import com.client.core.model.Cocrdsl;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: COCRDSL
 * @citation app/bms/COCRDSL.bms
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CocrdslRepository extends JpaRepository<Cocrdsl, String> {
}
