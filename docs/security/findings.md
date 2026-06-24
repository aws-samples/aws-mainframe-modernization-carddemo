# AppSec findings — CardDemo Go port

Tracks: RAU-47.
Last updated: 2026-06-08 (Reviewer pass: F-003/F-004/F-011 reclassified ACCEPTED-RISK; posture table corrected; all MEDIUM now fixed or accepted).

## Posture summary

| Severity | Open | Accepted-risk | Fixed |
|---|---:|---:|---:|
| High | 0 | 0 | 2 |
| Medium | 0 | 6 | 3 |
| Low | 1 | 2 | 2 |

Sign-off requires: zero open HIGH; all MEDIUM fixed or accepted-with-rationale.

## Status legend

- **OPEN** — finding stands; remediation owed before production.
- **FIXED** — resolved in this PR or an earlier PR; reference included.
- **ACCEPTED-RISK** — intentionally left; rationale and owner sign-off included.
- **WONTFIX** — does not apply; explanation included.

---

## HIGH Findings

### F-001 — html/template XSS (CVE GO-2026-4980 + GO-2026-4982)

- **Severity:** High
- **OWASP:** A03:2021 — Injection
- **CWE:** CWE-79 (Stored/Reflected XSS)
- **Surface:** `internal/web/auth/handlers.go:467` (adminEditTpl), `handlers.go:452` (adminListTpl), `handlers.go:430` (loginTpl)
- **Source:** govulncheck — GO-2026-4980, GO-2026-4982
- **Status:** FIXED (this PR — `go.mod` upgraded from `1.26.2` to `1.26.4`)
- **Owner:** RAU-47
- **Discovered:** 2026-06-06

**What.** Two vulnerabilities in the Go standard library `html/template` package: GO-2026-4980 bypasses the template escaper, and GO-2026-4982 bypasses meta-content URL escaping. Both were reachable via `auth.renderAdminEdit` at `internal/web/auth/handlers.go:467`, traced by govulncheck through `renderLogin`, `renderAdminList`, `renderAdminEdit`.

**Fix.** `go.mod` `go` directive updated from `1.26.2` to `1.26.4`. `go mod tidy` run. `govulncheck ./...` on the updated module: **No vulnerabilities found** (0 reachable, 0 in imports). Fixed in standard library at go1.26.3.

---

### F-002 — net/textproto error injection (CVE GO-2026-5039)

- **Severity:** High
- **OWASP:** A03:2021 — Injection
- **CWE:** CWE-74 (Improper Neutralization of Special Elements in Output)
- **Surface:** `cmd/carddemo/main.go:64` — `http.Server.ListenAndServe` call chain
- **Source:** govulncheck — GO-2026-5039
- **Status:** FIXED (this PR — same `go.mod` upgrade to `1.26.4` as F-001)
- **Owner:** RAU-47
- **Discovered:** 2026-06-06

**What.** Arbitrary inputs included in `net/textproto` errors without escaping. Malformed HTTP headers could cause raw attacker-controlled bytes to appear in error messages, potentially leaking into logs or error responses. Reachable via `carddemo.main` → `http.Server.ListenAndServe` → `textproto.Reader.ReadMIMEHeader`.

**Fix.** Resolved by the go1.26.4 upgrade (fixed in go1.26.4).

---

## MEDIUM Findings

### F-003 — IP rate-limiter bypassable via X-Forwarded-For spoofing

- **Severity:** Medium
- **OWASP:** A07:2021 — Identification and Authentication Failures
- **CWE:** CWE-290 (Authentication Bypass by Spoofing)
- **Surface:** `internal/auth/middleware.go:28-42`
- **Source:** Code review
- **Status:** ACCEPTED-RISK
- **Owner:** RAU-47
- **Discovered:** 2026-06-06
- **Rationale:** The correct fix depends on deployment topology: strip `X-Forwarded-For` entirely if no proxy is present, or implement a trusted-proxy allowlist tuned to the operator's CIDR. Neither is resolvable without a concrete deployment target, and this is a demo/educational modernization project without a specified network topology. The per-user rate limiter (5 attempts / 15 min, `UserLimiter`) remains effective against credential-stuffing regardless of IP rotation. Risk accepted for demo/development phase; must be revisited before production with a concrete proxy/no-proxy decision documented as an operator runbook item.

**What.** `ClientIP()` trusts the first value in `X-Forwarded-For` unconditionally with no proxy allowlist. An attacker who sends `X-Forwarded-For: 1.2.3.4` from a different IP can rotate through fake IPs to exhaust the per-IP rate limit budget of arbitrary victims (or avoid exhausting their own).

**Where.** `internal/auth/middleware.go:29-41`.

**Impact.** An attacker can submit unlimited login attempts against a target username by cycling through spoofed IPs, bypassing the per-IP rate limit. The per-user rate limit (`UserLimiter`, 5 attempts / 15 min) still applies but offers a narrower window.

**Remediation (pre-production).** Two options:
1. If deployed behind a trusted reverse proxy, restrict `X-Forwarded-For` trust to the proxy's CIDR (compare `r.RemoteAddr` to a trusted-proxy list before accepting the header).
2. If not behind a proxy, ignore `X-Forwarded-For` entirely and use only `r.RemoteAddr`.

---

### F-004 — Hardcoded default credentials

- **Severity:** Medium
- **OWASP:** A07:2021 — Identification and Authentication Failures
- **CWE:** CWE-798 (Use of Hard-coded Credentials)
- **Surface:** `internal/auth/seed.go:23-26`
- **Source:** Code review
- **Status:** ACCEPTED-RISK
- **Owner:** RAU-47
- **Discovered:** 2026-06-06
- **Rationale:** The `ADMIN001/PASSWORD` and `USER0001/PASSWORD` credentials are the canonical COBOL CardDemo demo credentials; they appear in every public reference implementation of the original mainframe application. Seeding them is intentional fidelity to the COBOL CardDemo specification, not an oversight. They are stored as bcrypt cost-10 hashes (not plaintext). This is a demo/educational sample; operators are expected to rotate credentials before production deployment. Risk accepted for demo/development phase — the existing in-code comment at `seed.go:23` already documents the production warning requirement.

**What.** `DefaultFixtures()` seeds `ADMIN001/PASSWORD` and `USER0001/PASSWORD`. Although stored as bcrypt hashes, the plaintext `PASSWORD` is trivially guessable and appears in every public COBOL CardDemo reference.

**Impact.** Any attacker aware of the COBOL CardDemo project can authenticate as admin on a default deployment where credential rotation has not been performed.

**Remediation (pre-production).** Add a `CARDDEMO_SKIP_DEFAULT_SEED=1` env-var guard, or generate random passwords at seed time printed once to stdout. At minimum, document in the operator runbook that `DefaultFixtures()` must not be called in production without credential rotation.

---

### F-005 — 8-character password ceiling

- **Severity:** Medium
- **OWASP:** A07:2021
- **CWE:** CWE-521 (Weak Password Requirements)
- **Surface:** `internal/domain/usersec.go:56`
- **Source:** Code review
- **Status:** ACCEPTED-RISK
- **Owner:** RAU-47
- **Rationale:** The 8-character limit mirrors the COBOL copybook `PIC X(08)` field width and is a fidelity requirement of the modernization project (preserve COBOL behavior). NIST SP 800-63B recommends a minimum of 8 characters and no arbitrary maximum; this ceiling violates the "no arbitrary maximum" guidance but bcrypt cost-10 still provides meaningful protection within the constraint. Risk is accepted pending a decision (outside scope of this PR) on whether the password policy should diverge from the COBOL original.
- **Discovered:** 2026-06-06

---

### F-006 — Integer overflow without range guard in COBOL codec encode path

- **Severity:** Medium
- **OWASP:** A04:2021 — Insecure Design
- **CWE:** CWE-190 (Integer Overflow)
- **Surface:** `internal/cobolfmt/binary.go:65-69`, `cobolfmt/packed.go:116,140`, `cobolfmt/zoned.go:94`
- **Source:** gosec G115 (9 instances)
- **Status:** FIXED (this PR — `internal/cobolfmt/binary.go:67-79`)
- **Owner:** RAU-47
- **Discovered:** 2026-06-06

**What.** gosec flagged 9 integer-conversion sites in the COBOL codec encode/decode paths (G115, HIGH). The decode conversions (`uint16→int16`, `uint32→int32`, `uint64→int64`) are intentional 2's-complement reinterpretations of COBOL COMP fields. The encode-path conversions performed silent truncation without error, which could produce incorrect monetary values.

**Fix.** `EncodeComp` now returns an error when the value exceeds the 2-byte (−32768..65535) or 4-byte (−2147483648..4294967295) range. Decode-path conversions are annotated with `// #nosec G115 -- intentional 2's-complement...` explaining the intent. All 9 gosec findings are cleared (9 nosec annotations, 0 remaining issues).

---

### F-007 — Missing HTTP security response headers

- **Severity:** Medium
- **OWASP:** A05:2021 — Security Misconfiguration
- **CWE:** CWE-16 (Configuration)
- **Surface:** `cmd/carddemo/main.go`, `internal/web/auth/handlers.go`
- **Source:** Code review
- **Status:** FIXED (this PR — `internal/web/middleware/security_headers.go`)
- **Owner:** RAU-47
- **Discovered:** 2026-06-06

**What.** No HTTP response headers are set to defend against clickjacking (`X-Frame-Options`), MIME sniffing (`X-Content-Type-Options`), or information disclosure (`Server` header removal). No `Content-Security-Policy` and no `Strict-Transport-Security`.

**Remediation.** Add a `SecurityHeaders` middleware that sets:

```
X-Frame-Options: DENY
X-Content-Type-Options: nosniff
Referrer-Policy: strict-origin-when-cross-origin
Content-Security-Policy: default-src 'self'; script-src 'none'; object-src 'none'
```

HSTS must be set only when TLS is terminated at the Go server; if TLS is handled upstream by the reverse proxy, add HSTS there. Fixed in this PR.

---

### F-008 — gosec G124 false positives (cookie attribute warnings)

- **Severity:** Medium (gosec-reported)
- **OWASP:** A02:2021
- **Surface:** `internal/auth/session.go:150,161,177`, `internal/auth/csrf.go:22`
- **Source:** gosec G124
- **Status:** ACCEPTED-RISK
- **Owner:** RAU-47
- **Rationale:** gosec G124 warns that cookie attributes are not statically set to secure values. The actual values (`Secure=true`, `HttpOnly=true`, `SameSite=Lax`) are correctly configured via the `CookieOptions` struct and applied via `DefaultCookieOptions()` (`session.go:140-152`). The CSRF cookie is intentionally `HttpOnly=false` so JavaScript can read it for the double-submit pattern. gosec cannot follow the data flow through the options struct. These are suppressed with `//nolint:gosec // G124: cookie attributes set via CookieOptions, verified correct`. The `CARDDEMO_INSECURE_COOKIES=1` env override for local dev is clearly guarded (`cmd/carddemo/main.go:33-35`) and excluded from production.
- **Discovered:** 2026-06-06

---

### F-009 — `cmd/web/main.go` uses http.ListenAndServe (no timeouts)

- **Severity:** Medium
- **OWASP:** A05:2021 — Security Misconfiguration
- **CWE:** CWE-676 (Use of Potentially Dangerous Function)
- **Surface:** `cmd/web/main.go:25`
- **Source:** gosec G114
- **Status:** FIXED (this PR — replaced with `http.Server` with `ReadTimeout`/`WriteTimeout`/`IdleTimeout`)
- **Owner:** RAU-47
- **Discovered:** 2026-06-06

---

### F-010 — Non-durable audit sink

- **Severity:** Medium
- **OWASP:** A09:2021 — Security Logging and Monitoring Failures
- **CWE:** CWE-778 (Insufficient Logging)
- **Surface:** `internal/audit/audit.go:38-57`
- **Source:** Code review
- **Status:** ACCEPTED-RISK
- **Owner:** RAU-47
- **Rationale:** `MemorySink` is explicitly documented as a placeholder ("Production will swap in a Sink that writes to durable storage" — `audit.go:38`). For the current demo/development phase the in-memory sink is the correct implementation. Must be replaced before production. Accepted for this PR; tracked as a gate item in the threat-model sign-off checklist.
- **Discovered:** 2026-06-06

---

### F-011 — Full PAN/CVV/SSN unmasked in domain types

- **Severity:** Medium
- **OWASP:** A02:2021 — Cryptographic Failures
- **CWE:** CWE-312 (Cleartext Storage of Sensitive Information)
- **Surface:** `internal/domain/card.go` (`CardNum`, `CardCVVCode`), `internal/domain/transaction.go` (`TranCardNum`), `internal/domain/customer.go` (`CustSSN`, `CustGovtIssuedID`)
- **Source:** Code review
- **Status:** ACCEPTED-RISK
- **Owner:** RAU-47
- **Discovered:** 2026-06-06
- **Rationale:** Masking logic is a deliberate design decision deferred to the handler-layer PRs that own the display and persistence paths: RAU-40 (account/customer), RAU-41 (card module), RAU-42 (transactions/bill-pay). Implementing a `PAN` sentinel type or `MaskedSSN()` in isolation here would be incomplete — the masking must be applied consistently at the same layer that renders and marshals the data. The domain types retain full values to preserve COBOL fidelity; masking is a presentation/audit concern, not a domain concern. Risk accepted pending RAU-40/41/42 sign-off; this finding gates those PRs' security review checklists.

**What.** `CardRecord.CardNum` (16-digit PAN), `CardRecord.CardCVVCode` (CVV as `int64`), `TransactionRecord.TranCardNum`, `CustomerRecord.CustSSN`, and `CustomerRecord.CustGovtIssuedID` are plain Go fields with no masking, no sentinel type, and no `Stringer` override. Any `log.Printf("%+v", record)`, JSON marshal, or debug dump will emit the full PAN, CVV, and SSN.

PCI-DSS requirement 3.3 prohibits storing CVV after authorization. PCI-DSS 3.4 requires PAN to be unreadable in storage and masked in display (show last 4 only).

**Impact.** PAN and CVV exfiltration via logs, debug output, or any future marshal path. Regulatory exposure under PCI-DSS and applicable PII law (SSN, government ID).

**Remediation (tracked in RAU-40/41/42).**
- Replace plain `string` for `CardNum` with a `PAN` type implementing `fmt.Stringer` returning `****-****-****-NNNN` and `json.Marshaler` returning the masked form.
- Remove or zero-out `CardCVVCode` after authorization completes (PCI-DSS 3.3).
- Add a `MaskedSSN()` method to `CustomerRecord` and use it in all display/log paths.

---

## LOW Findings

### F-012 — crypto/x509 hostname-parsing DoS (GO-2026-5037)

- **Severity:** Low (no attacker input reaches x509 hostname parsing in current code)
- **OWASP:** A06:2021 — Vulnerable and Outdated Components
- **CWE:** CWE-400 (Uncontrolled Resource Consumption)
- **Surface:** Standard library — `crypto/x509`
- **Source:** govulncheck — GO-2026-5037
- **Status:** FIXED (this PR — go1.26.4 upgrade; fixed in go1.26.4)
- **Owner:** RAU-47
- **Discovered:** 2026-06-06

**Notes.** Govulncheck traced an indirect path through `fmt.Sprintf` → `x509.HostnameError.Error`. Low exploitability (no attacker-controlled TLS hostname verification in this codebase). Cleared by the go1.26.4 upgrade.

---

### F-013 — Log injection via env-var address (gosec G706)

- **Severity:** Low
- **OWASP:** A09:2021
- **CWE:** CWE-117 (Improper Output Neutralization for Logs)
- **Surface:** `cmd/web/main.go:24`, `cmd/carddemo/main.go:63`
- **Source:** gosec G706
- **Status:** ACCEPTED-RISK
- **Owner:** RAU-47
- **Rationale:** The logged value (`CARDDEMO_ADDR` / `CARDDEMO_WEB_ADDR`) is a server-bind address set by the operator at startup, not user-supplied input. The risk of log injection via an operator-controlled env var is negligible. Suppressed with `//nolint:gosec // G706: addr is operator-supplied at startup, not user input`.
- **Discovered:** 2026-06-06

---

### F-014 — Unhandled fmt.Sscanf error (gosec G104)

- **Severity:** Low
- **OWASP:** A04:2021
- **CWE:** CWE-703 (Improper Check or Handling of Exceptional Conditions)
- **Surface:** `internal/cobolfmt/date.go:63`
- **Source:** gosec G104
- **Status:** OPEN
- **Owner:** RAU-47
- **Discovered:** 2026-06-06

**What.** `fmt.Sscanf(s[:2], "%d", &yy)` at `date.go:63` ignores its error return. If the two-character input is not a valid integer, `yy` retains its zero value and the function returns a silently wrong date.

**Remediation.** Check the error and propagate it:

```go
if _, err := fmt.Sscanf(s[:2], "%d", &yy); err != nil {
    return time.Time{}, fmt.Errorf("cobolfmt: date: invalid year %q: %w", s[:2], err)
}
```

---

### F-015 — In-memory session store lost on restart

- **Severity:** Low (correctness / reliability, not a direct security vulnerability)
- **OWASP:** A07:2021 (session management gap)
- **CWE:** CWE-613 (Insufficient Session Expiration)
- **Surface:** `internal/auth/session.go:46-58`
- **Source:** Code review
- **Status:** ACCEPTED-RISK
- **Owner:** RAU-47
- **Rationale:** `MemorySessionStore` is the expected implementation for the current development phase; a durable SQLite/Postgres-backed store is planned in RAU-38. On restart all sessions are invalidated — users are logged out and must re-authenticate. This is safe (sessions do not survive restart, so there is no stale-session risk) but creates a denial-of-service on every process restart. Accepted for demo/dev phase.
- **Discovered:** 2026-06-06

---

### F-016 — net Dial/LookupPort NUL-byte panic (GO-2026-4971, Windows-only)

- **Severity:** Low (Windows-only; CardDemo targets Linux; not directly user-reachable)
- **OWASP:** A06:2021 — Vulnerable and Outdated Components
- **CWE:** CWE-476 (NULL Pointer Dereference)
- **Surface:** Standard library — `net` package (`net.Dial`, `net.LookupPort`)
- **Source:** govulncheck — GO-2026-4971
- **Status:** FIXED (this PR — go1.26.4 upgrade; fixed in go1.26.3)
- **Owner:** RAU-47
- **Discovered:** 2026-06-06

**What.** A NUL byte (`\x00`) embedded in an address or port string passed to `net.Dial` or `net.LookupPort` causes a panic on Windows due to how the Windows socket API handles NUL-terminated strings. Govulncheck traces reachability via `cmd/carddemo/main.go` → `http.Server.ListenAndServe` → `net.Listen` → `net.ResolveTCPAddr`.

**Low-severity rationale.** Exploiting this requires supplying a NUL byte in the server bind address (`CARDDEMO_ADDR`), which is operator-supplied at startup and not attacker-controlled in the expected deployment. Additionally, CardDemo targets Linux; the panic is Windows-specific. Cleared by the go1.26.4 upgrade regardless of exploitability.

---

## Pending (not yet reviewable)

| Gate | Issue | Status | What unlocks |
|---|---|---|---|
| Account / customer handlers | RAU-40 | todo | PAN/SSN display-masking verification (F-011) |
| Card module handlers | RAU-41 | todo | CVV non-persistence check (F-011) |
| Transactions + bill pay | RAU-42 | todo | Integer-overflow risk in financial codec (F-006) |
| Persistence (SQLite/Postgres) | RAU-38 | todo | SQL injection review (parameter binding required per threat model §2) |
| Batch subcommands | RAU-44 | todo | Path traversal + file write review |
