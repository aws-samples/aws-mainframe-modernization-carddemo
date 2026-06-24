// Package middleware provides HTTP middleware for the CardDemo web server.
package middleware

import "net/http"

// SecurityHeaders sets defensive response headers on every response.
// Addresses F-007 (RAU-47): missing X-Frame-Options, X-Content-Type-Options, CSP, Referrer-Policy.
// HSTS is intentionally omitted here — it must be set by the TLS-terminating reverse proxy.
func SecurityHeaders(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		h := w.Header()
		h.Set("X-Frame-Options", "DENY")
		h.Set("X-Content-Type-Options", "nosniff")
		h.Set("Referrer-Policy", "strict-origin-when-cross-origin")
		h.Set("Content-Security-Policy", "default-src 'self'; script-src 'none'; object-src 'none'; base-uri 'self'; form-action 'self'")
		next.ServeHTTP(w, r)
	})
}
