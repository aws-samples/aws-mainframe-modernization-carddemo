package com.carddemo.repo;

import com.carddemo.domain.Account;
import jakarta.persistence.LockModeType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Optional;

public interface AccountRepository extends JpaRepository<Account, Long> {

    /**
     * SELECT ... FOR UPDATE (plan R3): the REST replacement for the CICS
     * {@code READ UPDATE} that COBIL00C issues on ACCTDAT (READ-ACCTDAT-FILE,
     * line 351) — serializes concurrent bill payments on the same account.
     */
    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("SELECT a FROM Account a WHERE a.accountId = :id")
    Optional<Account> findByIdForUpdate(@Param("id") Long id);
}
