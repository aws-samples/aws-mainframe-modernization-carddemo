// Package domain holds CardDemo domain types ported from COBOL copybooks.
package domain

import (
	"errors"
	"strings"
)

// UserType mirrors SEC-USR-TYPE in CSUSR01Y.cpy: 'A' = admin, 'U' = user.
type UserType byte

const (
	UserTypeAdmin UserType = 'A'
	UserTypeUser  UserType = 'U'
)

func (t UserType) Valid() bool { return t == UserTypeAdmin || t == UserTypeUser }

func (t UserType) String() string {
	switch t {
	case UserTypeAdmin:
		return "ADMIN"
	case UserTypeUser:
		return "USER"
	}
	return ""
}

// UserSec is the Go port of SEC-USER-DATA (CSUSR01Y.cpy, 80 bytes).
//
// COBOL layout:
//
//	05 SEC-USR-ID     PIC X(08)
//	05 SEC-USR-FNAME  PIC X(20)
//	05 SEC-USR-LNAME  PIC X(20)
//	05 SEC-USR-PWD    PIC X(08)   -- on the mainframe; in Go we never store cleartext
//	05 SEC-USR-TYPE   PIC X(01)
//	05 SEC-USR-FILLER PIC X(23)
//
// PwdHash replaces the cleartext PIC X(08) password field. The legacy 8-char
// limit is enforced on input (UserID, plaintext password) but the persisted
// hash is bcrypt — there is no fixed-width password column in Go-land.
type UserSec struct {
	UserID    string
	FirstName string
	LastName  string
	PwdHash   string
	Type      UserType
}

// Field length limits taken straight from the copybook.
const (
	UserIDMaxLen    = 8
	FirstNameMaxLen = 20
	LastNameMaxLen  = 20
	PasswordMaxLen  = 8
)

var (
	ErrUserIDEmpty     = errors.New("user id is required")
	ErrUserIDTooLong   = errors.New("user id exceeds 8 characters")
	ErrFirstNameEmpty  = errors.New("first name is required")
	ErrLastNameEmpty   = errors.New("last name is required")
	ErrPasswordEmpty   = errors.New("password is required")
	ErrPasswordTooLong = errors.New("password exceeds 8 characters")
	ErrInvalidUserType = errors.New("user type must be 'A' (admin) or 'U' (user)")
)

// NormalizeUserID matches COBOL FUNCTION UPPER-CASE on SEC-USR-ID: trim, upper.
func NormalizeUserID(id string) string {
	return strings.ToUpper(strings.TrimSpace(id))
}

// ValidateUserInput validates the human-supplied parts of a UserSec record
// (everything except the password hash, which is produced separately).
func ValidateUserInput(userID, first, last string, t UserType) error {
	id := strings.TrimSpace(userID)
	if id == "" {
		return ErrUserIDEmpty
	}
	if len(id) > UserIDMaxLen {
		return ErrUserIDTooLong
	}
	if strings.TrimSpace(first) == "" {
		return ErrFirstNameEmpty
	}
	if strings.TrimSpace(last) == "" {
		return ErrLastNameEmpty
	}
	if !t.Valid() {
		return ErrInvalidUserType
	}
	return nil
}

// ValidatePassword enforces the legacy 8-char ceiling on the *plaintext* the
// caller supplies. The stored bcrypt hash is unbounded.
func ValidatePassword(pw string) error {
	if pw == "" {
		return ErrPasswordEmpty
	}
	if len(pw) > PasswordMaxLen {
		return ErrPasswordTooLong
	}
	return nil
}
