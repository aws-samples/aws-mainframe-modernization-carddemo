package com.client.core.repository;

import com.client.core.model.AwsM2CardemoFtpTest;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Spring Data JPA Repository for: AWS.M2.CARDEMO.FTP.TEST
 * @citation AWS.M2.CARDEMO.FTP.TEST (source not found)
 * Target: PostgreSQL on Cloud SQL (schema from the approved Agent 07 plan).
 */
@Repository
public interface AwsM2CardemoFtpTestRepository extends JpaRepository<AwsM2CardemoFtpTest, String> {
}
