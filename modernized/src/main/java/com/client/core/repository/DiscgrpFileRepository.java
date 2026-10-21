package com.client.core.repository;

import com.client.core.model.DiscgrpFile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: DISCGRP_FILE
 * @citation DISCGRP_FILE (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface DiscgrpFileRepository extends JpaRepository<DiscgrpFile, String> {
}
