package com.client.core.repository;

import com.client.core.model.Cipausmy;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: CIPAUSMY
 * @citation app/app-authorization-ims-db2-mq/cpy/CIPAUSMY.cpy
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface CipausmyRepository extends JpaRepository<Cipausmy, String> {
}
