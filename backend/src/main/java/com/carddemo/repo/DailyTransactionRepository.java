package com.carddemo.repo;

import com.carddemo.model.DailyTransaction;
import org.springframework.data.jpa.repository.JpaRepository;

public interface DailyTransactionRepository extends JpaRepository<DailyTransaction, String> {
}
