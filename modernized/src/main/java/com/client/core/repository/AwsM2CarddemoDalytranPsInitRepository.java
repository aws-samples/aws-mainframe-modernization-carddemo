package com.client.core.repository;

import com.client.core.model.AwsM2CarddemoDalytranPsInit;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: AWS.M2.CARDDEMO.DALYTRAN.PS.INIT
 * @citation AWS.M2.CARDDEMO.DALYTRAN.PS.INIT (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface AwsM2CarddemoDalytranPsInitRepository extends JpaRepository<AwsM2CarddemoDalytranPsInit, String> {
}
