package auth

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/audit"
	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/domain"
	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/repo"
)

func newTestService(t *testing.T) (*Service, *audit.MemorySink, *repo.InMemoryUserSec) {
	t.Helper()
	users := repo.NewInMemoryUserSec()
	sink := audit.NewMemorySink()
	store := NewMemorySessionStore()
	svc := NewService(users, store, sink)
	// Tighten the windows so per-test waits are short, and use bcrypt cost 4
	// to keep tests fast.
	svc.UserLimiter = NewRateLimiter(3, time.Minute)
	svc.IPLimiter = NewRateLimiter(20, time.Minute)
	if err := SeedDefaultUsers(context.Background(), users, 4); err != nil {
		t.Fatalf("seed: %v", err)
	}
	return svc, sink, users
}

func TestLoginSuccessAndAudit(t *testing.T) {
	svc, sink, _ := newTestService(t)
	sess, err := svc.Login(context.Background(), "ADMIN001", "PASSWORD", "1.2.3.4")
	if err != nil {
		t.Fatalf("login: %v", err)
	}
	if sess.UserID != "ADMIN001" || !sess.IsAdmin() {
		t.Fatalf("unexpected session: %+v", sess)
	}
	if got := sink.CountByAction(audit.ActionLoginSuccess); got != 1 {
		t.Fatalf("expected 1 success audit, got %d", got)
	}
}

func TestLoginUserCaseInsensitive(t *testing.T) {
	svc, _, _ := newTestService(t)
	sess, err := svc.Login(context.Background(), "  admin001  ", "PASSWORD", "1.2.3.4")
	if err != nil {
		t.Fatalf("login: %v", err)
	}
	if sess.UserID != "ADMIN001" {
		t.Fatalf("expected normalized ADMIN001, got %q", sess.UserID)
	}
}

func TestLoginWrongPasswordDoesNotLeakUserExistence(t *testing.T) {
	svc, sink, _ := newTestService(t)
	_, err1 := svc.Login(context.Background(), "ADMIN001", "wrong", "1.2.3.4")
	_, err2 := svc.Login(context.Background(), "NOSUCH", "anything", "1.2.3.4")
	if !errors.Is(err1, ErrPasswordMismatch) || !errors.Is(err2, ErrPasswordMismatch) {
		t.Fatalf("both should return ErrPasswordMismatch, got %v / %v", err1, err2)
	}
	// The audit log carries Reason which differentiates the two for ops, but
	// the public error must be identical.
	if got := sink.CountByAction(audit.ActionLoginFailure); got != 2 {
		t.Fatalf("expected 2 failure audits, got %d", got)
	}
}

// TestBruteForceRateLimit is the acceptance-criteria test from RAU-39.
//
// Scenario: an attacker hammers ADMIN001 with bad passwords from one IP. After
// the per-user limit (3) is exhausted, even the *correct* password must be
// rejected with ErrRateLimited. After the window passes, login succeeds again.
func TestBruteForceRateLimit(t *testing.T) {
	svc, sink, _ := newTestService(t)

	// Pin time so we can advance the rate-limit window deterministically.
	now := time.Unix(1_700_000_000, 0)
	svc.UserLimiter.now = func() time.Time { return now }
	svc.IPLimiter.now = func() time.Time { return now }

	for i := 0; i < 3; i++ {
		_, err := svc.Login(context.Background(), "ADMIN001", "wrong", "1.2.3.4")
		if !errors.Is(err, ErrPasswordMismatch) {
			t.Fatalf("attempt %d: expected ErrPasswordMismatch, got %v", i, err)
		}
	}

	// The 4th attempt — even with the correct password — must be locked out.
	_, err := svc.Login(context.Background(), "ADMIN001", "PASSWORD", "1.2.3.4")
	if !errors.Is(err, ErrRateLimited) {
		t.Fatalf("expected ErrRateLimited after 3 failures, got %v", err)
	}

	// Audit must have recorded the block, not a successful login.
	if got := sink.CountByAction(audit.ActionLoginSuccess); got != 0 {
		t.Fatalf("brute-force lockout must not produce a success audit, got %d", got)
	}
	if got := sink.CountByAction(audit.ActionLoginFailure); got < 4 {
		t.Fatalf("expected at least 4 failure audits, got %d", got)
	}
	// At least one of the failure events must reference the rate-limit reason.
	sawRL := false
	for _, e := range sink.Events() {
		if e.Action == audit.ActionLoginFailure && strings.Contains(e.Reason, "rate limited") {
			sawRL = true
			break
		}
	}
	if !sawRL {
		t.Fatal("expected an audit event with reason mentioning rate limit")
	}

	// Advance past the window — now the legitimate user can sign in.
	now = now.Add(2 * time.Minute)
	sess, err := svc.Login(context.Background(), "ADMIN001", "PASSWORD", "1.2.3.4")
	if err != nil {
		t.Fatalf("expected login to succeed after window expiry, got %v", err)
	}
	if !sess.IsAdmin() {
		t.Fatalf("expected admin session, got %+v", sess)
	}
}

func TestSuccessfulLoginResetsUserCounter(t *testing.T) {
	svc, _, _ := newTestService(t)
	for i := 0; i < 2; i++ {
		_, _ = svc.Login(context.Background(), "ADMIN001", "wrong", "1.2.3.4")
	}
	// Below the threshold — correct password succeeds.
	if _, err := svc.Login(context.Background(), "ADMIN001", "PASSWORD", "1.2.3.4"); err != nil {
		t.Fatalf("login: %v", err)
	}
	// Counter should be reset; another 2 failures must not lock out.
	for i := 0; i < 2; i++ {
		_, err := svc.Login(context.Background(), "ADMIN001", "wrong", "1.2.3.4")
		if errors.Is(err, ErrRateLimited) {
			t.Fatalf("attempt %d should not be rate limited after success reset", i)
		}
	}
}

func TestCreateUserHashesPassword(t *testing.T) {
	svc, _, users := newTestService(t)
	if err := svc.CreateUser(context.Background(), "ADMIN001", "OPERATOR", "Op", "Erator", "secret", domain.UserTypeUser, "1.2.3.4"); err != nil {
		t.Fatalf("create: %v", err)
	}
	u, err := users.Get(context.Background(), "OPERATOR")
	if err != nil {
		t.Fatalf("get: %v", err)
	}
	if u.PwdHash == "secret" || strings.Contains(u.PwdHash, "secret") {
		t.Fatal("stored password must be a bcrypt hash, not cleartext")
	}
	if err := VerifyPassword(u.PwdHash, "secret"); err != nil {
		t.Fatalf("hash should verify: %v", err)
	}
}

func TestUpdateUserKeepsHashWhenPasswordBlank(t *testing.T) {
	svc, _, users := newTestService(t)
	before, _ := users.Get(context.Background(), "USER0001")
	if err := svc.UpdateUser(context.Background(), "ADMIN001", "USER0001", "Reg", "User", "", domain.UserTypeUser, "1.2.3.4"); err != nil {
		t.Fatalf("update: %v", err)
	}
	after, _ := users.Get(context.Background(), "USER0001")
	if before.PwdHash != after.PwdHash {
		t.Fatal("blank password must not rotate the hash")
	}
	if after.FirstName != "Reg" || after.LastName != "User" {
		t.Fatalf("name fields not updated: %+v", after)
	}
}

func TestSeedIsIdempotent(t *testing.T) {
	users := repo.NewInMemoryUserSec()
	if err := SeedDefaultUsers(context.Background(), users, 4); err != nil {
		t.Fatalf("seed1: %v", err)
	}
	if err := SeedDefaultUsers(context.Background(), users, 4); err != nil {
		t.Fatalf("seed2: %v", err)
	}
	all, _ := users.List(context.Background())
	if len(all) != 2 {
		t.Fatalf("expected 2 seeded users, got %d", len(all))
	}
	for _, u := range all {
		if u.PwdHash == "PASSWORD" || strings.Contains(u.PwdHash, "PASSWORD") {
			t.Fatalf("seeded password for %s must be hashed, got %q", u.UserID, u.PwdHash)
		}
	}
}
