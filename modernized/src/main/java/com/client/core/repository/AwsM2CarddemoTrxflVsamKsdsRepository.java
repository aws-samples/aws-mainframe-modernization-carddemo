package com.client.core.repository;

import com.client.core.model.AwsM2CarddemoTrxflVsamKsds;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: AWS.M2.CARDDEMO.TRXFL.VSAM.KSDS
 * @citation AWS.M2.CARDDEMO.TRXFL.VSAM.KSDS (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface AwsM2CarddemoTrxflVsamKsdsRepository extends JpaRepository<AwsM2CarddemoTrxflVsamKsds, String> {
}
