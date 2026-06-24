package auth

import (
	"context"
	"crypto/rand"
	"encoding/base64"
	"errors"
	"net/http"
	"sync"
	"time"

	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/domain"
)

// SessionCookieName is the cookie that carries the opaque session id. The id
// is server-side only; the cookie holds nothing else (no claims, no JWT).
const SessionCookieName = "carddemo_sid"

// CSRFCookieName is the cookie that carries the CSRF token for double-submit.
// The same value also appears in a hidden form field; the middleware compares
// the two in constant time.
const CSRFCookieName = "carddemo_csrf"

// CSRFFormField / CSRFHeader are the input names checked on state-changing
// requests. Either may carry the token.
const (
	CSRFFormField = "csrf_token"
	CSRFHeader    = "X-CSRF-Token"
)

// Session is a server-side record. The cookie value is just the opaque ID.
type Session struct {
	ID        string
	UserID    string
	UserType  domain.UserType
	CSRFToken string
	CreatedAt time.Time
	ExpiresAt time.Time
}

func (s Session) IsAdmin() bool { return s.UserType == domain.UserTypeAdmin }

// SessionStore is the interface for the server-side session table. Production
// will back this with Redis or similar; the in-memory impl is fine for dev and
// tests.
type SessionStore interface {
	New(ctx context.Context, userID string, t domain.UserType, ttl time.Duration) (Session, error)
	Get(ctx context.Context, id string) (Session, error)
	Delete(ctx context.Context, id string) error
}

var (
	ErrSessionNotFound = errors.New("session not found")
	ErrSessionExpired  = errors.New("session expired")
)

type MemorySessionStore struct {
	mu       sync.Mutex
	sessions map[string]Session
	now      func() time.Time
	rand     func() (string, error)
}

func NewMemorySessionStore() *MemorySessionStore {
	return &MemorySessionStore{
		sessions: make(map[string]Session),
		now:      time.Now,
		rand:     randomToken,
	}
}

func (s *MemorySessionStore) New(_ context.Context, userID string, t domain.UserType, ttl time.Duration) (Session, error) {
	id, err := s.rand()
	if err != nil {
		return Session{}, err
	}
	csrf, err := s.rand()
	if err != nil {
		return Session{}, err
	}
	now := s.now()
	sess := Session{
		ID:        id,
		UserID:    userID,
		UserType:  t,
		CSRFToken: csrf,
		CreatedAt: now,
		ExpiresAt: now.Add(ttl),
	}
	s.mu.Lock()
	s.sessions[id] = sess
	s.mu.Unlock()
	return sess, nil
}

func (s *MemorySessionStore) Get(_ context.Context, id string) (Session, error) {
	s.mu.Lock()
	sess, ok := s.sessions[id]
	s.mu.Unlock()
	if !ok {
		return Session{}, ErrSessionNotFound
	}
	if !sess.ExpiresAt.IsZero() && s.now().After(sess.ExpiresAt) {
		// Drop expired sessions on read so the store self-cleans.
		s.mu.Lock()
		delete(s.sessions, id)
		s.mu.Unlock()
		return Session{}, ErrSessionExpired
	}
	return sess, nil
}

func (s *MemorySessionStore) Delete(_ context.Context, id string) error {
	s.mu.Lock()
	delete(s.sessions, id)
	s.mu.Unlock()
	return nil
}

func randomToken() (string, error) {
	var b [32]byte
	if _, err := rand.Read(b[:]); err != nil {
		return "", err
	}
	return base64.RawURLEncoding.EncodeToString(b[:]), nil
}

// CookieOptions controls how Set-Cookie headers are produced. Defaults satisfy
// the issue's security non-negotiables: Secure, HttpOnly, SameSite=Lax. Tests
// may toggle Secure off to keep httptest happy.
type CookieOptions struct {
	Secure   bool
	SameSite http.SameSite
	Path     string
	Domain   string
}

func DefaultCookieOptions() CookieOptions {
	return CookieOptions{
		Secure:   true,
		SameSite: http.SameSiteLaxMode,
		Path:     "/",
	}
}

// SetSessionCookie writes the session-id cookie. The session cookie is always
// HttpOnly (no JS access). The CSRF cookie is intentionally NOT HttpOnly so
// JS-driven forms can mirror it back.
func SetSessionCookie(w http.ResponseWriter, sess Session, opts CookieOptions) {
	http.SetCookie(w, &http.Cookie{
		Name:     SessionCookieName,
		Value:    sess.ID,
		Path:     opts.Path,
		Domain:   opts.Domain,
		Expires:  sess.ExpiresAt,
		MaxAge:   int(time.Until(sess.ExpiresAt).Seconds()),
		Secure:   opts.Secure,
		HttpOnly: true,
		SameSite: opts.SameSite,
	})
	http.SetCookie(w, &http.Cookie{
		Name:     CSRFCookieName,
		Value:    sess.CSRFToken,
		Path:     opts.Path,
		Domain:   opts.Domain,
		Expires:  sess.ExpiresAt,
		MaxAge:   int(time.Until(sess.ExpiresAt).Seconds()),
		Secure:   opts.Secure,
		HttpOnly: false,
		SameSite: opts.SameSite,
	})
}

// ClearSessionCookies expires both cookies immediately.
func ClearSessionCookies(w http.ResponseWriter, opts CookieOptions) {
	for _, name := range []string{SessionCookieName, CSRFCookieName} {
		http.SetCookie(w, &http.Cookie{
			Name:     name,
			Value:    "",
			Path:     opts.Path,
			Domain:   opts.Domain,
			Expires:  time.Unix(0, 0),
			MaxAge:   -1,
			Secure:   opts.Secure,
			HttpOnly: name == SessionCookieName,
			SameSite: opts.SameSite,
		})
	}
}
