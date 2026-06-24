package auth

import (
	"errors"
	"strings"
	"testing"
)

func TestHashAndVerifyPassword(t *testing.T) {
	hash, err := HashPasswordCost("PASSWORD", 4)
	if err != nil {
		t.Fatalf("hash: %v", err)
	}
	if hash == "PASSWORD" || strings.Contains(hash, "PASSWORD") {
		t.Fatal("bcrypt hash must not contain plaintext")
	}
	if err := VerifyPassword(hash, "PASSWORD"); err != nil {
		t.Fatalf("verify success: %v", err)
	}
	if err := VerifyPassword(hash, "wrong"); !errors.Is(err, ErrPasswordMismatch) {
		t.Fatalf("expected mismatch sentinel, got %v", err)
	}
}

func TestVerifyPasswordCollapsesAllErrorsToMismatch(t *testing.T) {
	// A garbage hash must still produce ErrPasswordMismatch (no leakage of the
	// underlying bcrypt-format error to upstream).
	if err := VerifyPassword("not-a-bcrypt-hash", "anything"); !errors.Is(err, ErrPasswordMismatch) {
		t.Fatalf("expected ErrPasswordMismatch, got %v", err)
	}
}
