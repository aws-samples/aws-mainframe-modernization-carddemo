package com.client.core.repository;

import com.client.core.model.AwsM2CarddemoCardxrefVsamKsds;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS
 * @citation AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface AwsM2CarddemoCardxrefVsamKsdsRepository extends JpaRepository<AwsM2CarddemoCardxrefVsamKsds, String> {
}
