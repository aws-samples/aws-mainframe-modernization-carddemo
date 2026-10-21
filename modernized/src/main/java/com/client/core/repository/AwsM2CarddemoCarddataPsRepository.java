package com.client.core.repository;

import com.client.core.model.AwsM2CarddemoCarddataPs;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: AWS.M2.CARDDEMO.CARDDATA.PS
 * @citation AWS.M2.CARDDEMO.CARDDATA.PS (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface AwsM2CarddemoCarddataPsRepository extends JpaRepository<AwsM2CarddemoCarddataPs, String> {
}
