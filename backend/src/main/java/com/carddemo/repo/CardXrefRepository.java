package com.carddemo.repo;

import com.carddemo.model.CardXref;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface CardXrefRepository extends JpaRepository<CardXref, String> {
    Optional<CardXref> findFirstByAcctId(Long acctId);
}
