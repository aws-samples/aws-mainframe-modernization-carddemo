package com.carddemo.repo;

import com.carddemo.model.TransactionRecord;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface TransactionRepository extends JpaRepository<TransactionRecord, String> {

    Page<TransactionRecord> findAllByOrderByTranIdAsc(Pageable pageable);

    List<TransactionRecord> findByCardNumOrderByOrigTsAsc(String cardNum);

    /** Transactions whose origination date (first 10 chars of TRAN-ORIG-TS)
     *  falls within [from, to] inclusive, mirroring CORPT00C's range check. */
    @Query("select t from TransactionRecord t where substring(t.origTs, 1, 10) between :from and :to order by t.origTs, t.tranId")
    List<TransactionRecord> findInDateRange(@Param("from") String from, @Param("to") String to);
}
