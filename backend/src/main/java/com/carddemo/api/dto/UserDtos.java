package com.carddemo.api.dto;

import com.carddemo.domain.UserAccount;

/** Admin user CRUD shapes — COUSR00C-03C (plan §6). Password never returned. */
public final class UserDtos {

    private UserDtos() {
    }

    /** User row/detail — never carries the password (plan §5). */
    public record UserView(
            String userId,
            String firstName,
            String lastName,
            String userType,
            long version) {

        public static UserView of(UserAccount u) {
            return new UserView(trim(u.getUserId()), trim(u.getFirstName()),
                    trim(u.getLastName()), trim(u.getUserType()), u.getVersion());
        }

        private static String trim(String s) {
            return s == null ? null : s.trim();
        }
    }

    /** POST /users — COUSR01C add-user fields (all mandatory). */
    public record UserCreateRequest(
            String userId,
            String firstName,
            String lastName,
            String password,
            String userType) {
    }

    /**
     * PUT /users/{id} — COUSR02C update. Password blank/null keeps the stored
     * hash (the admin edit screen backs the ONLY password-change path, plan §6).
     */
    public record UserUpdateRequest(
            Long version,
            String firstName,
            String lastName,
            String password,
            String userType) {
    }
}
