package com.carddemo.service;

import com.carddemo.model.UserSecurity;
import com.carddemo.repo.UserSecurityRepository;
import com.carddemo.web.ApiException;
import com.carddemo.web.Dto;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** COUSR00C (list, 10/screen), COUSR01C (add), COUSR02C (update), COUSR03C (delete). */
@Service
public class UserAdminService {

    public static final int PAGE_SIZE = 10;

    private final UserSecurityRepository users;

    public UserAdminService(UserSecurityRepository users) {
        this.users = users;
    }

    public Dto.PageResult<Dto.UserView> list(int page) {
        var p = users.findAllByOrderByUsrIdAsc(PageRequest.of(page, PAGE_SIZE));
        return Dto.PageResult.of(p.map(Dto.UserView::of));
    }

    public Dto.UserView get(String userId) {
        return Dto.UserView.of(find(userId));
    }

    @Transactional
    public Dto.UserView add(Dto.UserRequest req) {
        required(req.firstName(), "First Name can NOT be empty...");
        required(req.lastName(), "Last Name can NOT be empty...");
        required(req.userId(), "User ID can NOT be empty...");
        required(req.password(), "Password can NOT be empty...");
        required(req.userType(), "User Type can NOT be empty...");
        if (!req.userType().equalsIgnoreCase("A") && !req.userType().equalsIgnoreCase("U")) {
            throw ApiException.badRequest("Invalid value. Valid values are (A/U)...");
        }
        String id = req.userId().trim().toUpperCase();
        if (id.length() > 8) {
            throw ApiException.badRequest("User ID must be 8 characters or less...");
        }
        if (users.existsById(id)) {
            throw ApiException.badRequest("User ID already exist...");
        }
        UserSecurity u = new UserSecurity();
        u.setUsrId(id);
        apply(u, req);
        users.save(u);
        return Dto.UserView.of(u);
    }

    @Transactional
    public Dto.UserView update(String userId, Dto.UserRequest req) {
        UserSecurity u = find(userId);
        if (req.firstName() != null) {
            required(req.firstName(), "First Name can NOT be empty...");
        }
        if (req.lastName() != null) {
            required(req.lastName(), "Last Name can NOT be empty...");
        }
        if (req.password() != null) {
            required(req.password(), "Password can NOT be empty...");
        }
        if (req.userType() != null) {
            required(req.userType(), "User Type can NOT be empty...");
            if (!req.userType().equalsIgnoreCase("A") && !req.userType().equalsIgnoreCase("U")) {
                throw ApiException.badRequest("Invalid value. Valid values are (A/U)...");
            }
        }
        apply(u, req);
        users.save(u);
        return Dto.UserView.of(u);
    }

    @Transactional
    public void delete(String userId) {
        UserSecurity u = find(userId);
        users.delete(u);
    }

    private UserSecurity find(String userId) {
        if (userId == null || userId.isBlank()) {
            throw ApiException.badRequest("User ID can NOT be empty...");
        }
        return users.findById(userId.trim().toUpperCase())
                .orElseThrow(() -> ApiException.notFound("User ID NOT found..."));
    }

    private static void apply(UserSecurity u, Dto.UserRequest req) {
        if (req.firstName() != null) u.setFirstName(req.firstName().trim());
        if (req.lastName() != null) u.setLastName(req.lastName().trim());
        if (req.password() != null) u.setPassword(req.password().trim());
        if (req.userType() != null) u.setUsrType(req.userType().trim().toUpperCase());
    }

    private static void required(String v, String message) {
        if (v == null || v.isBlank()) {
            throw ApiException.badRequest(message);
        }
    }
}
