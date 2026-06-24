# ADR 0004 — Persistence: replacing VSAM KSDS

**Status:** Accepted (RAU-36)

## Context

CardDemo uses five VSAM KSDS files as its primary data store:
`USRSEC`, `ACCTDAT`, `CARDDAT`, `CUSTDAT`, `TRANSACT`, and `CARDXREF`.
VSAM is mainframe-only and cannot be used in a Go application.

## Decision

**Single relational backend, accessed through repository interfaces only.**

- Development and integration tests: `modernc.org/sqlite` (pure Go, no CGO required).
- Production: `github.com/jackc/pgx/v5/stdlib` (PostgreSQL-compatible driver).
- All SQL is confined to `internal/repo`; no raw SQL in handlers or services.
- Repository interfaces are defined in `internal/repo`; implementations satisfy those interfaces.
  Services import the interface, not the concrete type — swapping SQLite for Postgres requires
  only a one-line change in the wiring code.

## Alternatives considered

- Pure in-memory maps: already used for tests in RAU-39; not suitable for persistence or multi-instance.
- GORM: adds a reflection-heavy ORM layer that obscures SQL and makes migrations harder to reason about.
- Direct Postgres only: CGO-free SQLite lets integration tests run without a running Postgres instance.

## Consequences

- Schema migrations live in `internal/repo/migrations/` (SQL files, applied by the repo init path).
- The test harness creates an in-memory SQLite DB per test using `modernc.org/sqlite`.
- Production deploys set `DATABASE_URL` to a Postgres DSN; the wiring in `cmd/web/main.go` switches driver.
