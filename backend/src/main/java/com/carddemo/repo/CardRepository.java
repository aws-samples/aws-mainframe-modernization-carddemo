package com.carddemo.repo;

import com.carddemo.model.Card;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

public interface CardRepository extends JpaRepository<Card, String> {
    Page<Card> findAllByOrderByCardNumAsc(Pageable pageable);
}
