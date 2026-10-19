package com.client.core.repository;

import com.client.core.model.Cocrdli;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: COCRDLI
 * @citation app/bms/COCRDLI.bms
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CocrdliRepository extends JpaRepository<Cocrdli, String> {
}
