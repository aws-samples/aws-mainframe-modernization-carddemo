package com.client.core.repository;

import com.client.core.model.AwsM2CarddemoCarddataVsamKsds;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS
 * @citation AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface AwsM2CarddemoCarddataVsamKsdsRepository extends JpaRepository<AwsM2CarddemoCarddataVsamKsds, String> {
}
