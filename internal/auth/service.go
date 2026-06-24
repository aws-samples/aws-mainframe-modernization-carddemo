package auth

import (
	"context"
	"errors"
	"strings"
	"time"

	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/audit"
	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/domain"
	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/repo"
)

// DefaultSessionTTL is the lifetime of a fresh session. Production may want
// shorter for admin sessions; we keep one knob for simplicity.
const DefaultSessionTTL = 8 * time.Hour

// DefaultLoginAttemptsPerWindow / DefaultLoginWindow are the rate-limit
// defaults. The numbers are deliberately conservative — five tries inside a
// fifteen-minute window per IP and per user. The handler trips on either.
const (
	DefaultLoginAttemptsPerWindow = 5
	DefaultLoginWindow            = 15 * time.Minute
)

// ErrRateLimited is returned when an IP or username is over budget.
var ErrRateLimited = errors.New("too many login attempts")

// Service is the authn/authz layer over a UserSec repository. Handlers depend
// on this; tests use it directly without going through HTTP.
type Service struct {
	Users       repo.UserSecRepo
	Sessions    SessionStore
	Audit       audit.Sink
	IPLimiter   *RateLimiter
	UserLimiter *RateLimiter
	SessionTTL  time.Duration
}

// NewService wires sensible defaults around a repo + store.
func NewService(users repo.UserSecRepo, store SessionStore, sink audit.Sink) *Service {
	return &Service{
		Users:       users,
		Sessions:    store,
		Audit:       sink,
		IPLimiter:   NewRateLimiter(DefaultLoginAttemptsPerWindow*4, DefaultLoginWindow),
		UserLimiter: NewRateLimiter(DefaultLoginAttemptsPerWindow, DefaultLoginWindow),
		SessionTTL:  DefaultSessionTTL,
	}
}

// Login validates credentials, applies per-IP and per-user rate limiting,
// audits the attempt, and on success returns a fresh server-side session.
//
// On any failure the audit Reason is intentionally generic so the response
// path can return a non-leaky message to the client.
func (s *Service) Login(ctx context.Context, userID, password, remote string) (Session, error) {
	id := domain.NormalizeUserID(userID)
	ipKey := "ip:" + remote
	userKey := "user:" + id

	// Pre-check both limiters before doing any work — bcrypt is expensive and
	// we don't want an attacker to keep us busy after they're already locked.
	if ok, _ := s.IPLimiter.Peek(ipKey); !ok {
		s.audit(audit.Event{Action: audit.ActionLoginFailure, Target: id, Remote: remote, Reason: "ip rate limited"})
		return Session{}, ErrRateLimited
	}
	if ok, _ := s.UserLimiter.Peek(userKey); !ok {
		s.audit(audit.Event{Action: audit.ActionLoginFailure, Target: id, Remote: remote, Reason: "user rate limited"})
		return Session{}, ErrRateLimited
	}

	user, err := s.Users.Get(ctx, id)
	if err != nil {
		s.recordFailure(ipKey, userKey)
		s.audit(audit.Event{Action: audit.ActionLoginFailure, Target: id, Remote: remote, Reason: "unknown user"})
		return Session{}, ErrPasswordMismatch
	}

	if err := VerifyPassword(user.PwdHash, password); err != nil {
		s.recordFailure(ipKey, userKey)
		s.audit(audit.Event{Action: audit.ActionLoginFailure, Target: id, Remote: remote, Reason: "wrong password"})
		return Session{}, ErrPasswordMismatch
	}

	// Success: clear the user counter so a single typo doesn't compound, but
	// leave the IP counter alone — high IP volume across users is itself
	// suspicious.
	s.UserLimiter.Reset(userKey)

	sess, err := s.Sessions.New(ctx, user.UserID, user.Type, s.SessionTTL)
	if err != nil {
		s.audit(audit.Event{Action: audit.ActionLoginFailure, Target: id, Remote: remote, Reason: "session create failed"})
		return Session{}, err
	}
	s.audit(audit.Event{Action: audit.ActionLoginSuccess, Actor: user.UserID, Target: user.UserID, Remote: remote})
	return sess, nil
}

func (s *Service) Logout(ctx context.Context, sess Session, remote string) error {
	err := s.Sessions.Delete(ctx, sess.ID)
	s.audit(audit.Event{Action: audit.ActionLogout, Actor: sess.UserID, Target: sess.UserID, Remote: remote})
	return err
}

func (s *Service) recordFailure(ipKey, userKey string) {
	s.IPLimiter.Allow(ipKey)
	s.UserLimiter.Allow(userKey)
}

func (s *Service) audit(e audit.Event) {
	if s.Audit != nil {
		s.Audit.Record(e)
	}
}

// CreateUser hashes the plaintext password and inserts a new UserSec record.
// Returns ErrConflict from repo if the id is taken.
func (s *Service) CreateUser(ctx context.Context, actor, userID, first, last, password string, t domain.UserType, remote string) error {
	if err := domain.ValidateUserInput(userID, first, last, t); err != nil {
		return err
	}
	if err := domain.ValidatePassword(password); err != nil {
		return err
	}
	hash, err := HashPassword(password)
	if err != nil {
		return err
	}
	id := domain.NormalizeUserID(userID)
	u := domain.UserSec{
		UserID:    id,
		FirstName: strings.TrimSpace(first),
		LastName:  strings.TrimSpace(last),
		PwdHash:   hash,
		Type:      t,
	}
	if err := s.Users.Create(ctx, u); err != nil {
		return err
	}
	s.audit(audit.Event{Action: audit.ActionUserCreate, Actor: actor, Target: id, Remote: remote})
	return nil
}

// UpdateUser updates first/last/type and, if newPassword is non-empty, the
// hash. It never touches an existing hash unless explicitly asked.
func (s *Service) UpdateUser(ctx context.Context, actor, userID, first, last, newPassword string, t domain.UserType, remote string) error {
	if err := domain.ValidateUserInput(userID, first, last, t); err != nil {
		return err
	}
	id := domain.NormalizeUserID(userID)
	existing, err := s.Users.Get(ctx, id)
	if err != nil {
		return err
	}
	existing.FirstName = strings.TrimSpace(first)
	existing.LastName = strings.TrimSpace(last)
	existing.Type = t
	if newPassword != "" {
		if err := domain.ValidatePassword(newPassword); err != nil {
			return err
		}
		hash, err := HashPassword(newPassword)
		if err != nil {
			return err
		}
		existing.PwdHash = hash
	}
	if err := s.Users.Update(ctx, existing); err != nil {
		return err
	}
	s.audit(audit.Event{Action: audit.ActionUserUpdate, Actor: actor, Target: id, Remote: remote})
	return nil
}

func (s *Service) DeleteUser(ctx context.Context, actor, userID, remote string) error {
	id := domain.NormalizeUserID(userID)
	if err := s.Users.Delete(ctx, id); err != nil {
		return err
	}
	s.audit(audit.Event{Action: audit.ActionUserDelete, Actor: actor, Target: id, Remote: remote})
	return nil
}

func (s *Service) ListUsers(ctx context.Context) ([]domain.UserSec, error) {
	return s.Users.List(ctx)
}

func (s *Service) GetUser(ctx context.Context, userID string) (domain.UserSec, error) {
	return s.Users.Get(ctx, userID)
}
