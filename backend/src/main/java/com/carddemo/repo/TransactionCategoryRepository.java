package com.carddemo.repo;

import com.carddemo.model.TransactionCategory;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TransactionCategoryRepository extends JpaRepository<TransactionCategory, TransactionCategory.Key> {
}
