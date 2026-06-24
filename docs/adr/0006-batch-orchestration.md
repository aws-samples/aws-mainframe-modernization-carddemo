# ADR 0006 — Batch orchestration: replacing JCL

**Status:** Accepted (RAU-36)

## Context

CardDemo's batch workload is driven by JCL job streams (POSTTRAN.jcl, INTCALC.jcl, etc.).
JCL is mainframe-specific and cannot run in a Go application.

## Decision

**`cmd/batch` CLI with subcommands, plus a thin YAML orchestrator.**

- Each JCL step is a Go function in `internal/batch/` invoked as a `cmd/batch <subcommand>`.
- A YAML config file (`configs/orchestrator.yaml`) declares job sequences and their step order,
  replacing JCL EXEC and DD card sequencing. A thin orchestrator reads the YAML and runs steps
  in order, capturing return codes.
- No daemon or scheduler is built; external schedulers (cron, AWS Batch, Step Functions) invoke
  `cmd/batch` subcommands directly.

## Alternatives considered

- Keep JCL and use a JCL interpreter: adds a heavy dependency; the programs themselves still need porting.
- One binary per JCL job: multiplies build artifacts; a single `cmd/batch` with subcommands is simpler.
- Embedded Go script (e.g., `gopher/script`): adds a scripting runtime for what is essentially a list of steps.

## Consequences

- Each batch function has a clear input (file path or DB query) and return code (0 = success, non-zero = fail).
- Integration tests run individual batch functions with an in-memory SQLite DB (no JCL, no file system).
- The YAML orchestrator is `configs/orchestrator.yaml.example`; production copies override it.
