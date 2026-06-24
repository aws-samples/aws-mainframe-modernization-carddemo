package auth

import (
	"context"
	"fmt"

	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/domain"
	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/repo"
)

// SeedFixture is a single legacy-style record in the seed list. Password is
// the plaintext from the legacy fixtures; SeedDefaultUsers replaces it with a
// bcrypt hash before writing to the repo.
type SeedFixture struct {
	UserID    string
	FirstName string
	LastName  string
	Password  string
	Type      domain.UserType
}

// DefaultFixtures mirror the legacy seed:
//
//	ADMIN001 / PASSWORD  (admin)
//	USER0001 / PASSWORD  (regular user)
//
// These are the credentials documented in COSGN00C.cbl test runs. The seed
// step replaces the cleartext PIC X(08) password with a bcrypt hash so no
// cleartext credential is ever persisted.
func DefaultFixtures() []SeedFixture {
	return []SeedFixture{
		{UserID: "ADMIN001", FirstName: "Admin", LastName: "User", Password: "PASSWORD", Type: domain.UserTypeAdmin},
		{UserID: "USER0001", FirstName: "Regular", LastName: "User", Password: "PASSWORD", Type: domain.UserTypeUser},
	}
}

// SeedDefaultUsers inserts the default fixtures into the given repo, hashing
// each password with bcrypt at the supplied cost. Idempotent: an existing
// user_id is skipped (not overwritten).
func SeedDefaultUsers(ctx context.Context, r repo.UserSecRepo, cost int) error {
	if cost == 0 {
		cost = DefaultBcryptCost
	}
	for _, f := range DefaultFixtures() {
		if _, err := r.Get(ctx, f.UserID); err == nil {
			continue
		}
		hash, err := HashPasswordCost(f.Password, cost)
		if err != nil {
			return fmt.Errorf("hash %s: %w", f.UserID, err)
		}
		u := domain.UserSec{
			UserID:    domain.NormalizeUserID(f.UserID),
			FirstName: f.FirstName,
			LastName:  f.LastName,
			PwdHash:   hash,
			Type:      f.Type,
		}
		if err := r.Create(ctx, u); err != nil {
			return fmt.Errorf("create %s: %w", f.UserID, err)
		}
	}
	return nil
}
