# ADR 0003 — HTTP framework

**Status:** Accepted (RAU-36)

## Context

The online CICS application drives BMS screens via terminal I/O. The Go port needs an
HTTP router and middleware stack to replace that interaction model. Standard library
`net/http` is sufficient for handlers; routing (URL params, grouped middleware) benefits
from a thin layer.

## Decision

**stdlib `net/http` + `github.com/go-chi/chi/v5`.**

- All handlers implement `http.Handler` — no framework lock-in.
- chi provides URL parameter extraction (`chi.URLParam`), route grouping, and
  middleware chaining (auth, CSRF, logging) without reflection magic.
- No SPA framework; `html/template` renders each BMS screen replacement (ADR 0005).

## Alternatives considered

- `gin`: heavier, opinionated on error handling; chi is closer to stdlib.
- `echo`: similar to gin; adds a custom context type that complicates handler testing.
- Pure `net/http` with `ServeMux`: path-parameter routing is verbose; chi is worth the dependency.

## Consequences

- Handlers can be tested with `httptest.NewRecorder` + `http.NewRequest` without a running server.
- chi middleware (auth gate, CSRF, request-ID) is wired in `internal/web` and composed in `cmd/web/main.go`.
