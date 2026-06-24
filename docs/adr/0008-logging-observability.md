# ADR 0008 — Logging and observability baseline

**Status:** Accepted (RAU-36)

## Context

The mainframe emits SMF records and CICS journal entries for observability.
The Go port needs a structured logging baseline that is lightweight and composable.
Deeper observability (traces, metrics) is deferred to a later issue.

## Decision

**`log/slog` (Go stdlib structured logger), request-scoped via `context.Context`.**

- The default handler is `slog.NewJSONHandler(os.Stdout, nil)` in production,
  `slog.NewTextHandler(os.Stderr, nil)` in development (`LOG_FORMAT=text` env var).
- Handlers attach a request-scoped logger to `context.Context` via a middleware;
  all log calls within a request use `slog.FromCtx(ctx)`.
- Log levels: DEBUG (local dev), INFO (production default), WARN / ERROR for exceptions.
- Audit events go through `internal/audit` (separate sink), not `slog`.

## Alternatives considered

- `zerolog`: fastest JSON logger, but adds a dependency for a feature stdlib now covers.
- `zap`: high-performance, two APIs (sugared / typed); `slog` is simpler and sufficient.
- `logrus`: older, reflection-based; `slog` is the stdlib successor.

## Consequences

- No logging dependency to manage; `slog` is in the standard library since Go 1.21.
- Context propagation means log fields (request-ID, user-ID) appear on every line within a request.
- Metrics and distributed traces are out of scope for the current phase.
