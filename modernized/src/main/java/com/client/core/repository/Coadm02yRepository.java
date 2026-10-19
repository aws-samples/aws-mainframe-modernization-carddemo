package com.client.core.repository;

import com.client.core.model.Coadm02y;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: COADM02Y
 * @citation app/cpy/COADM02Y.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface Coadm02yRepository extends JpaRepository<Coadm02y, String> {
}
