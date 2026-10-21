package com.client.core.repository;

import com.client.core.model.AwsM2CarddemoUsrsecVsamEsds;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: AWS.M2.CARDDEMO.USRSEC.VSAM.ESDS
 * @citation AWS.M2.CARDDEMO.USRSEC.VSAM.ESDS (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface AwsM2CarddemoUsrsecVsamEsdsRepository extends JpaRepository<AwsM2CarddemoUsrsecVsamEsds, String> {
}
