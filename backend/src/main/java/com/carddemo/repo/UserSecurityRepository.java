package com.carddemo.repo;

import com.carddemo.model.UserSecurity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

public interface UserSecurityRepository extends JpaRepository<UserSecurity, String> {
    Page<UserSecurity> findAllByOrderByUsrIdAsc(Pageable pageable);
}
