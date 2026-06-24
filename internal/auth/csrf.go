package auth

import (
	"crypto/subtle"
	"net/http"
	"time"
)

// NewCSRFToken returns a fresh random token suitable for double-submit.
func NewCSRFToken() (string, error) {
	return randomToken()
}

// SetPreLoginCSRFCookie writes a pre-login CSRF cookie under the same name as
// the session-bound cookie. This way the CSRFProtect middleware enforces
// double-submit on POST /login without a separate code path, and the cookie is
// transparently overwritten by the session-bound value at login time.
//
// It is intentionally NOT HttpOnly so a JS-driven login form can mirror the
// value into the hidden field.
func SetPreLoginCSRFCookie(w http.ResponseWriter, token string, opts CookieOptions) {
	http.SetCookie(w, &http.Cookie{
		Name:     CSRFCookieName,
		Value:    token,
		Path:     opts.Path,
		Domain:   opts.Domain,
		Expires:  time.Now().Add(15 * time.Minute),
		MaxAge:   int((15 * time.Minute).Seconds()),
		Secure:   opts.Secure,
		HttpOnly: false,
		SameSite: opts.SameSite,
	})
}

// ConstantTimeEqual is a length-aware constant-time string compare.
func ConstantTimeEqual(a, b string) bool {
	if len(a) != len(b) {
		return false
	}
	return subtle.ConstantTimeCompare([]byte(a), []byte(b)) == 1
}
