package com.client.core.repository;

import com.client.core.model.Psbpautl;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: PSBPAUTL
 * @citation PSBPAUTL (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface PsbpautlRepository extends JpaRepository<Psbpautl, String> {
}
