package auth

import (
	"context"
	"crypto/subtle"
	"errors"
	"net"
	"net/http"
	"strings"

	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/audit"
	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/domain"
)

type ctxKey int

const sessionCtxKey ctxKey = 1

// SessionFromContext returns the authenticated session, or false if the
// request is anonymous.
func SessionFromContext(ctx context.Context) (Session, bool) {
	s, ok := ctx.Value(sessionCtxKey).(Session)
	return s, ok
}

func withSession(ctx context.Context, s Session) context.Context {
	return context.WithValue(ctx, sessionCtxKey, s)
}

// ClientIP extracts a best-effort remote IP. Trusts X-Forwarded-For only when
// the caller wired the middleware behind a proxy that sets it; otherwise we
// fall back to RemoteAddr.
func ClientIP(r *http.Request) string {
	if xf := r.Header.Get("X-Forwarded-For"); xf != "" {
		// Take the first entry — leftmost is the original client.
		for i := 0; i < len(xf); i++ {
			if xf[i] == ',' {
				return xf[:i]
			}
		}
		return xf
	}
	host, _, err := net.SplitHostPort(r.RemoteAddr)
	if err != nil {
		return r.RemoteAddr
	}
	return host
}

// LoadSession is middleware that, if a valid session cookie is present,
// attaches the Session to the request context. It does NOT enforce auth — use
// RequireUser / RequireAdmin for that.
func LoadSession(store SessionStore) func(http.Handler) http.Handler {
	return func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			c, err := r.Cookie(SessionCookieName)
			if err == nil && c.Value != "" {
				sess, err := store.Get(r.Context(), c.Value)
				if err == nil {
					r = r.WithContext(withSession(r.Context(), sess))
				}
			}
			next.ServeHTTP(w, r)
		})
	}
}

// RequireUser forces the request to carry a valid session. Anonymous requests
// get 401 (API) or a 303 redirect to /login (browser).
func RequireUser(loginPath string) func(http.Handler) http.Handler {
	return func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			if _, ok := SessionFromContext(r.Context()); !ok {
				redirectOrUnauthorized(w, r, loginPath)
				return
			}
			next.ServeHTTP(w, r)
		})
	}
}

// RequireAdmin forces the session's UserType to be ADMIN. Non-admin users get
// 403 + an audit event so we can spot privilege-probing.
func RequireAdmin(sink audit.Sink, loginPath string) func(http.Handler) http.Handler {
	return func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			sess, ok := SessionFromContext(r.Context())
			if !ok {
				redirectOrUnauthorized(w, r, loginPath)
				return
			}
			if sess.UserType != domain.UserTypeAdmin {
				if sink != nil {
					sink.Record(audit.Event{
						Action: audit.ActionAccessDenied,
						Actor:  sess.UserID,
						Target: r.URL.Path,
						Remote: ClientIP(r),
						Reason: "non-admin attempted admin action",
					})
				}
				http.Error(w, "forbidden", http.StatusForbidden)
				return
			}
			next.ServeHTTP(w, r)
		})
	}
}

func redirectOrUnauthorized(w http.ResponseWriter, r *http.Request, loginPath string) {
	if wantsHTML(r) && loginPath != "" {
		http.Redirect(w, r, loginPath, http.StatusSeeOther)
		return
	}
	http.Error(w, "unauthorized", http.StatusUnauthorized)
}

func wantsHTML(r *http.Request) bool {
	a := r.Header.Get("Accept")
	return a == "" || strings.Contains(strings.ToLower(a), "text/html")
}

// CSRFProtect is middleware for state-changing requests (POST/PUT/DELETE/PATCH).
// It enforces double-submit: the token in the CSRFCookieName cookie must equal
// the token in the form field or X-CSRF-Token header. Constant-time compare.
//
// Safe methods (GET/HEAD/OPTIONS) pass through untouched.
func CSRFProtect() func(http.Handler) http.Handler {
	return func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			if isSafeMethod(r.Method) {
				next.ServeHTTP(w, r)
				return
			}
			cookie, err := r.Cookie(CSRFCookieName)
			if err != nil || cookie.Value == "" {
				http.Error(w, "missing csrf token", http.StatusForbidden)
				return
			}
			submitted := r.Header.Get(CSRFHeader)
			if submitted == "" {
				// Parse form lazily; ignore parse errors (handler may have a richer parser).
				_ = r.ParseForm()
				submitted = r.PostFormValue(CSRFFormField)
			}
			if submitted == "" {
				http.Error(w, "missing csrf token", http.StatusForbidden)
				return
			}
			if subtle.ConstantTimeCompare([]byte(submitted), []byte(cookie.Value)) != 1 {
				http.Error(w, "invalid csrf token", http.StatusForbidden)
				return
			}
			// Belt-and-braces: if there is a session, the cookie token must match the session's token too.
			if sess, ok := SessionFromContext(r.Context()); ok {
				if subtle.ConstantTimeCompare([]byte(cookie.Value), []byte(sess.CSRFToken)) != 1 {
					http.Error(w, "invalid csrf token", http.StatusForbidden)
					return
				}
			}
			next.ServeHTTP(w, r)
		})
	}
}

func isSafeMethod(m string) bool {
	return m == http.MethodGet || m == http.MethodHead || m == http.MethodOptions
}

// Common errors callers may want to detect.
var (
	ErrUnauthenticated = errors.New("unauthenticated")
	ErrForbidden       = errors.New("forbidden")
)
