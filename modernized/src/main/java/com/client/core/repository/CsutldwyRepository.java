package com.client.core.repository;

import com.client.core.model.Csutldwy;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CSUTLDWY
 * @citation app/cpy/CSUTLDWY.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CsutldwyRepository extends JpaRepository<Csutldwy, String> {
}
