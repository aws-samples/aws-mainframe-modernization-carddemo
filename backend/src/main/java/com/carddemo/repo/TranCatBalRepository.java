package com.carddemo.repo;

import com.carddemo.model.TranCatBal;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface TranCatBalRepository extends JpaRepository<TranCatBal, TranCatBal.Key> {
    List<TranCatBal> findAllByOrderByAcctIdAscTypeCdAscCatCdAsc();
}
