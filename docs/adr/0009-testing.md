# ADR 0009 — Testing strategy

**Status:** Accepted (RAU-36)

## Context

COBOL programs are tested via batch runs and CICS terminal scripts.
The Go port needs a testing strategy that gives fast feedback, covers the business
logic inherited from COBOL, and validates the data-layer without a running Postgres instance.

## Decision

**`testing` (stdlib) + `testify/assert` + `go-cmp`; integration tests use in-memory SQLite.**

- Unit tests use `testing` + `github.com/stretchr/testify/assert` for readable assertions.
- Deep struct comparison uses `github.com/google/go-cmp/cmp` (handles unexported fields, custom comparers).
- Integration tests that touch the repository layer spin up an in-memory `modernc.org/sqlite` DB per test
  (no Docker, no Postgres required in CI for the base suite).
- Golden-file tests for COBOL format conversions live in `internal/cobolfmt/testdata/`.
- Table-driven tests are preferred for COBOL-derived logic (many input/output rows).
- The `-race` flag is opt-in (`make test RACE=1`) because `modernc.org/sqlite` does not support `-race` under CGO-free mode; the race detector runs on non-DB test targets in CI.

## Consequences

- `go test ./...` passes on a clean checkout with no external services.
- `make test` runs the full suite; `make test RACE=1` adds the race detector for non-repo packages.
- Coverage targets: 80% line coverage for `internal/auth` and `internal/cobolfmt` at minimum.
