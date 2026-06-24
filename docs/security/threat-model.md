# Threat Model — CardDemo Go Port

Tracks: RAU-47 (AppSec review of the Go port).
Status: **reviewed against code as of 2026-06-06** (RAU-39 auth + RAU-42 transactions landed).

---

## 1. System overview

The CardDemo Go port replaces a COBOL/CICS/VSAM credit-card servicing app with a single Go module exposing:

| Surface | Component | Status |
|---|---|---|
| Online (HTTP/HTML) | `cmd/carddemo` + `internal/web/auth` | Auth + admin CRUD implemented (RAU-39) |
| Batch (CLI) | `cmd/batch` | Stub only — RAU-44 pending |
| Persistence | In-memory store (`internal/repo/usersec.go`) | SQLite/Postgres planned (RAU-38) |
| Domain / codecs | `internal/domain`, `internal/cobolfmt` | Implemented (RAU-37) |
| Identity | bcrypt password hashes, server-side sessions, ADMIN/USER roles | `internal/auth` — implemented |

External actors: cardholder users, admin users, batch operators (CI/cron). No third-party services on the request path.

---

## 2. Trust boundaries

```
[ Internet ] ──TLS──▶ [ Reverse proxy ] ──HTTP──▶ [ cmd/carddemo :8080 ]
                                                        │
                                              [ internal/web/auth ]
                                                        │
                                              [ internal/auth (session, CSRF, rate-limit) ]
                                                        │
                                              [ internal/repo (in-memory store) ]

[ Operator shell ] ──────────────────────▶ [ cmd/batch (stub) ]
```

Crossings (with current implementation status):

1. **Browser → web server**: untrusted input; auth, CSRF, session cookies protect this boundary.
2. **Web → repo**: in-memory store today; will be `database/sql` in RAU-38. Parameter binding required.
3. **Batch → filesystem**: reads EBCDIC fixtures. Path-traversal + write-permission risks (unimplemented, RAU-44).
4. **Operator shell → batch CLI**: privileged; secrets via env vars only.

---

## 3. Assets

| Asset | Sensitivity | Notes |
|---|---|---|
| User credentials (bcrypt hash + plaintext at login) | High | Covered by bcrypt + rate-limit. |
| Session cookies | High | Bearer of authenticated identity. |
| Card numbers (PAN) and CVV | High | PCI-relevant. Full PAN/CVV in domain layer — no masking yet. |
| Customer SSN, FICO, name/address | High | PII. No masking in domain types. |
| Account balances, transactions | Medium | Integrity critical; confidentiality moderate. |
| Audit log | Medium | In-memory only — volatile. |
| Admin user CRUD endpoints | High | Privilege escalation surface. |

---

## 4. STRIDE per surface

### 4.1 Authentication & session (`internal/auth`, `internal/web/auth`)

| Threat | Vector | Mitigation | Status |
|---|---|---|---|
| **S** — Credential stuffing | `POST /login` | bcrypt + per-IP and per-user rate-limiter (`service.go:38-39`) | Implemented |
| **S** — Session fixation | Attacker sets cookie pre-login | New session ID issued on login; old ID never carried forward | Implemented |
| **S** — Session hijack | Cookie sniffing | `Secure=true`, `HttpOnly=true`, `SameSite=Lax` via `DefaultCookieOptions()` | Implemented |
| **T** — CSRF | Cross-origin POST | Double-submit cookie pattern (`csrf.go`, `middleware.go:110-143`) | Implemented |
| **R** — Repudiation | "I didn't do that" | Audit log on login/logout/user-mutations (`service.go:50,67,78,121,150,158`) | Implemented (non-durable) |
| **I** — Password in logs | Error pages, audit events | `ErrPasswordMismatch` only; audit events never carry password material | Implemented |
| **D** — Slow-bcrypt DoS | Concurrent logins | Rate-limit (`DefaultLoginAttemptsPerWindow=5 / 15min`) | Implemented |
| **E** — User → admin escalation | Missing role check | `RequireAdmin` middleware on `/admin/*` (`cmd/carddemo/main.go:39,56`) | Implemented |

Open gaps confirmed by code review:

- **F-003**: `ClientIP()` trusts `X-Forwarded-For` header unconditionally (`middleware.go:29-41`). If the server is internet-facing without a trusted proxy, IP rate-limit is bypassable.
- **F-004**: Default seed credentials (`ADMIN001/PASSWORD`, `USER0001/PASSWORD`) are well-known COBOL-era defaults (`seed.go:24-25`). Must be rotated before any production use.
- **F-005**: Password ceiling is 8 characters (`domain/usersec.go:56`), a legacy COBOL field width. Weak by NIST SP 800-63B.
- **F-009**: `cmd/web/main.go` uses `http.ListenAndServe` (no timeout — gosec G114). Fixed in this PR.
- **F-012**: Missing HTTP security response headers (CSP, HSTS, X-Frame-Options, etc.).

### 4.2 Admin user CRUD (`/admin/*`)

| Threat | Vector | Mitigation | Status |
|---|---|---|---|
| **E** — Unprivileged access | GET/POST `/admin/*` by regular user | `RequireAdmin` middleware (`cmd/carddemo/main.go:39`) | Implemented |
| **T** — CSRF on mutations | Cross-origin POST to `/admin/users` | CSRF middleware wraps entire mux | Implemented |
| **T** — Over-long input | Huge first/last name field | `ValidateUserInput` enforces copybook limits | Implemented |
| **R** — Untracked mutations | Admin creates/updates/deletes without trace | Audit log on every mutation with actor+target+IP | Implemented (non-durable) |

Gap:

- **F-010**: Audit sink is `MemorySink` only — all events lost on process restart.

### 4.3 COBOL domain/codec layer (`internal/cobolfmt`, `internal/domain`)

| Threat | Vector | Mitigation | Status |
|---|---|---|---|
| **T** — Integer overflow in encode | Out-of-range financial value silently truncated | gosec G115 — no pre-encode range checks | GAP (F-006) |
| **I** — PAN/CVV/SSN in struct dumps | `fmt.Sprintf("%+v", record)` leaks PAN, CVV, SSN | No masking type or `Stringer` implementation | GAP (F-011, F-012, F-013) |

### 4.4 Dependency / supply chain

- Three external deps: `shopspring/decimal`, `golang.org/x/crypto`, `golang.org/x/text`.
- Standard library vulnerabilities in go1.26.2: see F-001, F-002, F-016 in findings.md.

---

## 5. Sign-off checklist

Items must be verified before production deployment:

- [ ] Zero open HIGH findings in `findings.md`
- [ ] All MEDIUM findings fixed or carrying accepted-risk rationale
- [ ] Go toolchain upgraded to ≥ 1.26.4 (resolves F-001, F-002, F-016)
- [ ] Default seed credentials rotated or seeding disabled via flag
- [ ] PAN/CVV masking layer reviewed for completeness when account/card/transaction handlers land (RAU-40–42)
- [ ] Durable audit sink implemented before production (F-010)
- [ ] Password policy reviewed against NIST SP 800-63B (F-005)
- [ ] IP extraction strategy confirmed: trusted-proxy config or removal of X-Forwarded-For trust (F-003)
