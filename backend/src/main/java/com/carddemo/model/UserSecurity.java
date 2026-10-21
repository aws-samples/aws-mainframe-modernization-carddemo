package com.carddemo.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.Setter;

/** CSUSR01Y.cpy — SEC-USER-DATA. */
@Entity
@Table(name = "user_security")
@Getter
@Setter
public class UserSecurity {

    /** SEC-USR-ID PIC X(08). */
    @Id
    @Column(name = "usr_id", length = 8)
    private String usrId;

    /** SEC-USR-FNAME PIC X(20). */
    @Column(name = "usr_fname", length = 20)
    private String firstName;

    /** SEC-USR-LNAME PIC X(20). */
    @Column(name = "usr_lname", length = 20)
    private String lastName;

    /** SEC-USR-PWD PIC X(08). */
    @Column(name = "usr_pwd", length = 8)
    private String password;

    /** SEC-USR-TYPE PIC X(01): 'A' = admin, 'U' = regular user. */
    @Column(name = "usr_type", length = 1)
    private String usrType;

    public boolean isAdmin() {
        return "A".equalsIgnoreCase(usrType);
    }
}
