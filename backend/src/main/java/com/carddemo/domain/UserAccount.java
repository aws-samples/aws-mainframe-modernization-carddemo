package com.carddemo.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Version;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

/** USRSEC (CSUSR01Y) — {@code users} table. Password is bcrypt, never returned. */
@Entity
@Table(name = "users")
public class UserAccount {

    @Id
    @Column(name = "user_id")
    private String userId;

    @Column(name = "first_name")
    private String firstName;

    @Column(name = "last_name")
    private String lastName;

    @Column(name = "password_hash")
    private String passwordHash;

    /** 'A' (admin) or 'U' (user) — CHECK-constrained. */
    @JdbcTypeCode(SqlTypes.CHAR)
    @Column(name = "user_type")
    private String userType;

    @Version
    private long version;

    protected UserAccount() {
        // JPA
    }

    /** COUSR01C add-user path (Phase 2). */
    public UserAccount(String userId, String firstName, String lastName,
                       String passwordHash, String userType) {
        this.userId = userId;
        this.firstName = firstName;
        this.lastName = lastName;
        this.passwordHash = passwordHash;
        this.userType = userType;
    }

    public String getUserId() {
        return userId;
    }

    public String getFirstName() {
        return firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public String getPasswordHash() {
        return passwordHash;
    }

    public String getUserType() {
        return userType;
    }

    // -- mutators for the COUSR02C update path (Phase 2) -------------------

    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    public void setPasswordHash(String passwordHash) {
        this.passwordHash = passwordHash;
    }

    public void setUserType(String userType) {
        this.userType = userType;
    }

    public long getVersion() {
        return version;
    }

    /** ADMIN|USER role name from user_type A|U (plan §5). */
    public String role() {
        return "A".equals(userType == null ? null : userType.trim()) ? "ADMIN" : "USER";
    }
}
