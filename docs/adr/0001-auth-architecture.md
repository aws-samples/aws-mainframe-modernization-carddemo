# ADR 0001 — Authentication & user management architecture (RAU-39)

## Status
Accepted (pending appsec sign-off, RAU-46).

## Context
The legacy CardDemo application authenticates via RACF + CICS sign-on map
(`COSGN00C.cbl` / `COSGN00.bms`) and manages users with four CICS programs
(`COUSR00C`/`01C`/`02C`/`03C`) reading and writing the `USRSEC` VSAM file
(`CSUSR01Y.cpy`, 80-byte record, 8-character cleartext password). RAU-39
replaces this with a Go HTTP application.

## Decision

### Authentication
- Server-side, opaque session IDs in `carddemo_sid` cookie (HttpOnly, Secure, SameSite=Lax).
- bcrypt password hashing (`golang.org/x/crypto/bcrypt`); compare via
  `bcrypt.CompareHashAndPassword` (constant-time).
- Per-IP **and** per-username rate limiting (fixed window, default 15-minute
  window with 5 attempts per user / 20 per IP). On lockout, even the correct
  password is rejected for the remainder of the window.
- Audit log entry on every login attempt (success or failure) and every admin
  user mutation. Audit `Reason` differentiates failure modes for ops; the
  public error is always the same (`invalid credentials`) so user existence
  does not leak.

### CSRF
- Double-submit cookie. The `carddemo_csrf` cookie is set by the server (not
  HttpOnly) and the value is mirrored in a hidden form field / `X-CSRF-Token`
  header. State-changing requests are rejected unless the two match in
  constant time.
- The same cookie name is used pre- and post-login: `GET /login` issues a
  short-lived token bound to the browser, and successful login replaces it
  with a session-bound token rotated on each new session.

### Authorization
- Role check on `domain.UserType` (`A` admin, `U` user). `RequireAdmin`
  middleware records `access.denied` audit events on probing.

### Persistence
- `internal/repo.UserSecRepo` interface — RAU-38 will provide the persistent
  implementation. The current `InMemoryUserSec` is sufficient for dev and
  tests.

### Seeding
- `auth.SeedDefaultUsers` replaces the legacy cleartext credentials
  (`ADMIN001/PASSWORD`, `USER0001/PASSWORD`) with bcrypt hashes during the
  seed step. No cleartext password ever reaches storage.

## Out of scope (future work)
- **OIDC / SAML federation** — explicitly excluded from this issue. When
  delivered, federated identities will live alongside the local UserSec store
  and reuse the same session/audit primitives. (Tracking with RAU-16 for Entra
  ID and RAU-25 for SSO setup docs.)
- **Self-service password reset** — explicitly excluded. The current admin
  UI lets administrators rotate any user's password; end-user reset will be
  added later with a separate token + email flow.

## Consequences
- The HTTP edge depends on bcrypt's cost; tests use cost 4 to keep them fast,
  production defaults to bcrypt's `DefaultCost` (10).
- `httptest` does not run TLS, so unit tests toggle `CookieOptions.Secure=false`.
  `auth.DefaultCookieOptions()` is verified in tests to ensure production
  cookies are always Secure.
- The in-memory rate limiter is per-process. If this is ever fronted by
  multiple replicas, swap `RateLimiter` for a shared store (Redis) without
  changing call sites.
