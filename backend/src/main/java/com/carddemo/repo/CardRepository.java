package com.carddemo.repo;

import com.carddemo.domain.Card;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface CardRepository extends JpaRepository<Card, String> {

    /**
     * Keyset page (plan §3 pagination pattern): replaces COCRDLIC
     * STARTBR/READNEXT — {@code WHERE card_number > :cursor ORDER BY card_number}.
     * Null accountId/cursor mean unfiltered/first page.
     */
    @Query("""
            SELECT c FROM Card c
            WHERE (:accountId IS NULL OR c.accountId = :accountId)
              AND (:cursor IS NULL OR c.cardNumber > :cursor)
            ORDER BY c.cardNumber ASC
            """)
    List<Card> pageForward(@Param("accountId") Long accountId,
                           @Param("cursor") String cursor,
                           Pageable pageable);

    List<Card> findByAccountIdOrderByCardNumberAsc(Long accountId);
}
