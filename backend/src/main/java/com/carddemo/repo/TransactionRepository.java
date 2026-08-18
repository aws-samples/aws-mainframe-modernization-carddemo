package com.carddemo.repo;

import com.carddemo.domain.Transaction;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface TransactionRepository extends JpaRepository<Transaction, String> {

    /**
     * Keyset page on transaction_id (COTRN00C paging). {@code fromId} is the
     * legacy "tran-id seek" — start listing at that id (inclusive); the cursor
     * is exclusive and wins when both are present (it encodes page position).
     */
    @Query("""
            SELECT t FROM Transaction t
            WHERE (:cursor IS NULL OR t.transactionId > :cursor)
              AND (:fromId IS NULL OR t.transactionId >= :fromId)
              AND (:cardNumber IS NULL OR t.cardNumber = :cardNumber)
            ORDER BY t.transactionId ASC
            """)
    List<Transaction> pageForward(@Param("cursor") String cursor,
                                  @Param("fromId") String fromId,
                                  @Param("cardNumber") String cardNumber,
                                  Pageable pageable);

    /**
     * Next transaction id (plan R2): PostgreSQL sequence replacing the legacy
     * READPREV-from-HIGH-VALUES max+1 (COTRN02C:444-451, COBIL00C:212-217),
     * which is race-prone. Callers render it as a 16-digit zero-padded string.
     */
    @Query(value = "SELECT nextval('transaction_id_seq')", nativeQuery = true)
    long nextTransactionId();
}
