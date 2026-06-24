# Architectural Decision Records

This directory contains the ADRs for the CardDemo Go port. Each record captures the context, options considered, and rationale for a design choice that would otherwise be hard to reconstruct from the code alone.

---

## Index

| # | Title | Status | Decision summary |
|---|---|---|---|
| [0001](0001-auth-architecture.md) | Authentication & User Management Architecture | Accepted | Replace RACF/CICS auth with server-side opaque session cookies, bcrypt password hashing, per-IP/per-username rate limiting, and double-submit CSRF tokens. Role is carried by `domain.UserType`. |
| [0002](0002-module-layout.md) | Go Module Layout | Accepted | Single root-level Go module (`github.com/aws-samples/aws-mainframe-modernization-carddemo`); entry points in `cmd/web/` and `cmd/batch/`; strict `cmd → service → domain` dependency direction. |
| [0003](0003-http-framework.md) | HTTP Framework | Accepted | stdlib `net/http` plus `github.com/go-chi/chi/v5` for routing and middleware. All handlers implement `http.Handler` to avoid framework lock-in. |
| [0004](0004-persistence.md) | Persistence — Replacing VSAM KSDS | Accepted | Single relational backend behind repository interfaces; `modernc.org/sqlite` for development/testing, `pgx/v5` for production Postgres. Flat EBCDIC binary files retained as the import/export format. |
| [0005](0005-templating-ui.md) | Templating and UI — BMS Screen Replacement | Accepted | Each BMS map replaced by one `html/template` HTML file embedded in the binary. No SPA, no JavaScript build step. Forms POST back over HTTP; server issues redirects. |
| [0006](0006-batch-orchestration.md) | Batch Orchestration — Replacing JCL | Accepted | JCL job streams replaced by a `cmd/batch` CLI whose subcommands map to JCL steps; orchestrated via `configs/orchestrator.yaml`; invoked by an external scheduler. No built-in daemon. |
| [0007](0007-decimal-arithmetic.md) | Decimal Arithmetic for COMP-3 and Monetary Fields | Accepted | `github.com/shopspring/decimal` for all monetary and COMP-3 fields. `float64` is banned for money. `internal/cobolfmt` provides `Comp3Decode`/`Comp3Encode` helpers. |
| [0008](0008-logging-observability.md) | Logging and Observability Baseline | Accepted | stdlib `log/slog` as the structured logger, propagated via `context.Context`. Audit events (login, logout, user CRUD) route to a separate `internal/audit` sink. |
| [0009](0009-testing.md) | Testing Strategy | Accepted | stdlib `testing` with `testify/assert` and `go-cmp`. In-memory SQLite for integration tests. Table-driven tests for COBOL-derived decode/encode logic. 80% line coverage target for `internal/auth` and `internal/cobolfmt`. |

---

## Status vocabulary

| Status | Meaning |
|---|---|
| Proposed | Under discussion; not yet binding |
| Accepted | Binding; implementation follows this decision |
| Superseded | Replaced by a later ADR (link in the record) |
| Deprecated | No longer applies; kept for history |

---

## Adding a new ADR

1. Copy an existing ADR file as `docs/adr/NNNN-short-title.md`.
2. Fill in Context, Options, Decision, and Consequences.
3. Add a row to the index table above.
4. Reference the ADR from the relevant code or issue where the decision is first applied.
