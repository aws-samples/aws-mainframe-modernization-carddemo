package com.client.core.repository;

import com.client.core.model.Psbpautb;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: PSBPAUTB
 * @citation PSBPAUTB (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface PsbpautbRepository extends JpaRepository<Psbpautb, String> {
}
