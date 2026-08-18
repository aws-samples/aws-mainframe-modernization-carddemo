package com.carddemo.api;

import com.carddemo.api.ValidationException.FieldError;
import com.carddemo.api.dto.ApiDtos.Page;
import com.carddemo.api.dto.UserDtos.UserCreateRequest;
import com.carddemo.api.dto.UserDtos.UserUpdateRequest;
import com.carddemo.api.dto.UserDtos.UserView;
import com.carddemo.domain.UserAccount;
import com.carddemo.repo.UserAccountRepository;
import org.springframework.data.domain.PageRequest;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

/**
 * COUSR00C-03C port (plan §4, §6): admin user CRUD.
 * <ul>
 *   <li>list — keyset paging on user_id (COUSR00C browse)</li>
 *   <li>create — COUSR01C non-empty edits, 8-char id/password (CSUSR01Y),
 *       type A/U; duplicate id → 409 "User ID already exist..." (DUPREC)</li>
 *   <li>update — COUSR02C: same non-empty edits; blank password keeps the
 *       stored hash (the admin edit is the ONLY password-change path);
 *       no-op → success without write ("Please modify to update ...")</li>
 *   <li>delete — COUSR03C read + delete</li>
 * </ul>
 * Passwords are bcrypt-hashed and never returned (plan §5).
 */
@Service
public class UserAdminService {

    /** COUSR01C DUPREC message. */
    static final String USER_ALREADY_EXISTS = "User ID already exist...";

    private final UserAccountRepository users;
    private final PasswordEncoder passwordEncoder;

    public UserAdminService(UserAccountRepository users, PasswordEncoder passwordEncoder) {
        this.users = users;
        this.passwordEncoder = passwordEncoder;
    }

    @Transactional(readOnly = true)
    public Page<UserView> listUsers(String cursor, Integer limit) {
        int pageSize = Paging.clamp(limit);
        List<UserAccount> page = users.pageForward(
                Paging.blankToNull(cursor), PageRequest.of(0, pageSize + 1));
        return Paging.page(page, pageSize, u -> u.getUserId().trim(), UserView::of);
    }

    @Transactional(readOnly = true)
    public UserView getUser(String userId) {
        return UserView.of(find(userId));
    }

    /** COUSR01C: mandatory-field edits, then WRITE with DUPREC → 409. */
    @Transactional
    public UserView createUser(UserCreateRequest req) {
        List<FieldError> errors = new ArrayList<>();
        String firstName = notEmpty(errors, "firstName", "First Name", req.firstName());
        String lastName = notEmpty(errors, "lastName", "Last Name", req.lastName());
        String userId = notEmpty(errors, "userId", "User ID", req.userId());
        String password = notEmpty(errors, "password", "Password", req.password());
        String userType = userType(errors, req.userType());
        if (userId != null && userId.length() > 8) {
            errors.add(new FieldError("userId", "User ID must be 8 characters or less..."));
        }
        if (password != null && password.length() > 8) {
            errors.add(new FieldError("password", "Password must be 8 characters or less..."));
        }
        if (!errors.isEmpty()) {
            throw new ValidationException(errors);
        }

        userId = userId.toUpperCase();
        if (users.existsById(userId)) {
            throw new ConflictException(USER_ALREADY_EXISTS);
        }
        UserAccount created = users.saveAndFlush(new UserAccount(
                userId, firstName, lastName, passwordEncoder.encode(password), userType));
        return UserView.of(created);
    }

    /** COUSR02C: non-empty edits; blank password = keep; no-op = no write. */
    @Transactional
    public UserView updateUser(String userId, UserUpdateRequest req) {
        List<FieldError> errors = new ArrayList<>();
        String firstName = notEmpty(errors, "firstName", "First Name", req.firstName());
        String lastName = notEmpty(errors, "lastName", "Last Name", req.lastName());
        String userType = userType(errors, req.userType());
        String password = req.password() == null || req.password().isBlank()
                ? null : req.password().trim();
        if (password != null && password.length() > 8) {
            errors.add(new FieldError("password", "Password must be 8 characters or less..."));
        }
        if (!errors.isEmpty()) {
            throw new ValidationException(errors);
        }

        UserAccount user = find(userId);
        if (req.version() != null && req.version() != user.getVersion()) {
            throw new ConflictException(AccountService.CHANGED_BY_SOMEONE_ELSE);
        }

        // COUSR02C "Please modify to update ..." — no-op returns without write
        boolean unchanged = firstName.equalsIgnoreCase(trim(user.getFirstName()))
                && lastName.equalsIgnoreCase(trim(user.getLastName()))
                && userType.equals(trim(user.getUserType()))
                && password == null;
        if (unchanged) {
            return UserView.of(user);
        }

        user.setFirstName(firstName);
        user.setLastName(lastName);
        user.setUserType(userType);
        if (password != null) {
            user.setPasswordHash(passwordEncoder.encode(password));
        }
        users.saveAndFlush(user);
        return UserView.of(user);
    }

    /** COUSR03C: read then delete by user id. */
    @Transactional
    public void deleteUser(String userId) {
        users.delete(find(userId));
    }

    private UserAccount find(String userId) {
        return users.findById(userId.toUpperCase())
                .orElseThrow(() -> new NotFoundException("User ID NOT found..."));
    }

    /** COUSR01C/02C "... can NOT be empty..." edits. */
    private static String notEmpty(List<FieldError> errors, String field, String label, String value) {
        if (value == null || value.isBlank()) {
            errors.add(new FieldError(field, label + " can NOT be empty..."));
            return null;
        }
        return value.trim();
    }

    /** users.user_type CHECK IN ('A','U') per CSUSR01Y. */
    private static String userType(List<FieldError> errors, String value) {
        if (value == null || value.isBlank()) {
            errors.add(new FieldError("userType", "User Type can NOT be empty..."));
            return null;
        }
        String v = value.trim().toUpperCase();
        if (!v.equals("A") && !v.equals("U")) {
            errors.add(new FieldError("userType", "User Type must be A or U"));
            return null;
        }
        return v;
    }

    private static String trim(String s) {
        return s == null ? "" : s.trim();
    }
}
