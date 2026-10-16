package com.client.core.repository;

import com.client.core.model.Csstrpfy;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CSSTRPFY
 * @citation app/cpy/CSSTRPFY.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CsstrpfyRepository extends JpaRepository<Csstrpfy, String> {
}
