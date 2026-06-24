# ADR 0002 — Go module layout

**Status:** Accepted (RAU-36)
**Decided:** 2026-06-02 (Connector ruling, resolving Option A vs B)

## Context

The original scaffold placed the Go module in a `carddemo-go/` subdirectory (Option B).
RAU-39 committed auth code at the repo root with module path
`github.com/aws-samples/aws-mainframe-modernization-carddemo` and go 1.26.2.
The Connector closed the layout question as Option A (root-level) based on:
- The RAU-36 spec says "go.mod at repo root".
- RAU-39's root module builds and tests pass on go 1.26.2; the `carddemo-go/` layout was never committed.
- Migrating RAU-39's working code into a subdir costs more than backfilling the scaffold around it.

## Decision

**Option A — root-level module.**

- Module path: `github.com/aws-samples/aws-mainframe-modernization-carddemo`
- `go.mod` at repo root, `go 1.26.2`.
- Entry points: `cmd/web/` (CICS online replacement) and `cmd/batch/` (JCL batch replacement).
  `cmd/carddemo/` from RAU-39 remains during the migration period; RAU-43 will consolidate into `cmd/web`.
- Internal packages: `internal/{domain,repo,service,web,batch,cobolfmt,auth,audit}`.
- Dependency direction: `cmd → service → domain`; `service → repo` (interface); `repo` imports `domain`; no cycles.

## Consequences

- All downstream issues (RAU-37 through RAU-45) import from `github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/...`.
- A future `cmd/carddemo` → `cmd/web` consolidation is a rename; it does not change module paths.
- ADR numbering: 0001 is RAU-39's auth-architecture; scaffold decisions start at 0002.
