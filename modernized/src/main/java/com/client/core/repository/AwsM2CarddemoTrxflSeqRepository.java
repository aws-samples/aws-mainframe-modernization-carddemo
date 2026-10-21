package com.client.core.repository;

import com.client.core.model.AwsM2CarddemoTrxflSeq;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: AWS.M2.CARDDEMO.TRXFL.SEQ
 * @citation AWS.M2.CARDDEMO.TRXFL.SEQ (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface AwsM2CarddemoTrxflSeqRepository extends JpaRepository<AwsM2CarddemoTrxflSeq, String> {
}
