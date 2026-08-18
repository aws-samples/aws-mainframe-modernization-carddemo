package com.carddemo.repo;

import com.carddemo.domain.UserAccount;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface UserAccountRepository extends JpaRepository<UserAccount, String> {

    /**
     * Keyset page on user_id — COUSR00C STARTBR/READNEXT replacement
     * (plan §3 pagination pattern). Null cursor means first page.
     */
    @Query("""
            SELECT u FROM UserAccount u
            WHERE (:cursor IS NULL OR u.userId > :cursor)
            ORDER BY u.userId ASC
            """)
    List<UserAccount> pageForward(@Param("cursor") String cursor, Pageable pageable);
}
