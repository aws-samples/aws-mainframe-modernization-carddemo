package auth

import (
	"errors"

	"golang.org/x/crypto/bcrypt"
)

// DefaultBcryptCost is bcrypt's default (10). Tests may pass a lower cost via
// HashPasswordCost to keep brute-force loops fast; production callers should
// always use HashPassword.
const DefaultBcryptCost = bcrypt.DefaultCost

// ErrPasswordMismatch signals a mismatched password without leaking whether
// the user existed. Callers MUST collapse "user not found" into the same
// error code path before logging.
var ErrPasswordMismatch = errors.New("invalid credentials")

func HashPassword(plain string) (string, error) {
	return HashPasswordCost(plain, DefaultBcryptCost)
}

func HashPasswordCost(plain string, cost int) (string, error) {
	h, err := bcrypt.GenerateFromPassword([]byte(plain), cost)
	if err != nil {
		return "", err
	}
	return string(h), nil
}

// VerifyPassword compares a bcrypt hash to a plaintext attempt in constant time
// (bcrypt.CompareHashAndPassword does the constant-time compare internally).
// Returns ErrPasswordMismatch on any mismatch so call sites can branch on a
// single sentinel.
func VerifyPassword(hash, plain string) error {
	if err := bcrypt.CompareHashAndPassword([]byte(hash), []byte(plain)); err != nil {
		return ErrPasswordMismatch
	}
	return nil
}
