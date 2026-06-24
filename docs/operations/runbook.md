# CardDemo Go Port — Operations Runbook

Reference for running, seeding, restoring, and troubleshooting the CardDemo Go port in a local or development environment.

---

## Table of contents

1. [Seed default data](#seed-default-data)
2. [Run the web server](#run-the-web-server)
3. [Run batch jobs](#run-batch-jobs)
4. [Restore application state](#restore-application-state)
5. [Common issues](#common-issues)
6. [Log locations](#log-locations)

---

## Seed default data

Default users are seeded **automatically at web server startup** via `internal/auth.SeedDefaultUsers`. No separate seed command is needed.

```bash
go run ./cmd/carddemo
# Output: carddemo listening on :8080
# Seeding happens in the first lines of startup; check logs if it fails.
```

After startup the following users are available:

| User ID | Password | Role |
|---|---|---|
| `ADMIN001` | `PASSWORD` | Admin |
| `USER0001` | `PASSWORD` | Regular user |

Seeding is idempotent: if a user ID already exists in the store, the seed step skips it silently.

> **Current limitation.** The user store is in-memory (`internal/repo.InMemoryUserSec`). All users — including any you create at runtime — are lost when the process exits. Persistent storage is planned under RAU-38.

### Seeding batch reference data

Batch reference files (transaction types, categories, disclosure groups) are read directly from the EBCDIC flat files in the `app/data/` directory. Copy the sample files to your working data directory before running any batch subcommand:

```bash
cp app/data/EBCDIC/* /your/data/dir/
```

---

## Run the web server

### Quickstart

```bash
go run ./cmd/carddemo
# → carddemo listening on :8080
```

> **`make run-web` vs `cmd/carddemo`:** `make run-web` starts `cmd/web`, which only serves `GET /healthz` and has no user seeding or login routes (stub pending RAU-43). Use `go run ./cmd/carddemo` for the working auth + admin server today.

Navigates to: `http://localhost:8080/login`

### Configuration

| Environment variable | Default | Purpose |
|---|---|---|
| `CARDDEMO_ADDR` | `:8080` | Listen address (`host:port`) |
| `CARDDEMO_INSECURE_COOKIES` | _(unset)_ | Set to `1` to disable Secure flag on cookies (HTTP/dev only) |

### Run on a different port

```bash
CARDDEMO_ADDR=:9090 go run ./cmd/carddemo
```

### Run over plain HTTP (local development)

The session and CSRF cookies are set with `Secure=true` by default, which prevents them being sent over HTTP. For local development without TLS:

```bash
CARDDEMO_INSECURE_COOKIES=1 go run ./cmd/carddemo
```

Do not set this in any environment where real users log in.

### Verify the server is running

```bash
curl -s http://localhost:8080/login | grep -i 'carddemo'
# Should return the login page HTML
```

---

## Run batch jobs

### List available subcommands

```bash
go run ./cmd/batch --help
```

Available subcommands and their legacy JCL equivalents:

| Subcommand | Replaces JCL | Description |
|---|---|---|
| `post-transaction` | `POSTTRAN.jcl` (CBTRN01C) | Core daily transaction processing |
| `categorize-transaction` | `TRANCATG.jcl` (CBTRN02C) | Transaction categorisation |
| `daily-rejects` | `DALYREJS.jcl` (CBTRN03C) | Daily rejects report |
| `account-file` | `ACCTFILE.jcl` (CBACT01C) | Account file refresh |
| `interest-calc` | `INTCALC.jcl` (CBACT02C) | Interest calculation |
| `tcat-balance` | `TCATBALF.jcl` (CBACT03C) | Transaction category balance |
| `read-account` | `READACCT.jcl` (CBACT04C) | Read account data |
| `customer-file` | `CUSTFILE.jcl` (CBCUS01C) | Customer file refresh |
| `create-statement` | `CREASTMT.JCL` (CBSTM03A) | Account statement generation |
| `report-file` | `REPTFILE.jcl` (CBSTM03B) | Report file generation |
| `export` | `CBEXPORT.jcl` (CBEXPORT) | Export to flat file |
| `import` | `CBIMPORT.jcl` (CBIMPORT) | Import from flat file |

### Run a single batch subcommand

```bash
go run ./cmd/batch post-transaction --input /data/transactions/daily.dat
go run ./cmd/batch interest-calc --month current
go run ./cmd/batch create-statement --output-dir /data/statements
```

### Run a multi-step job using the orchestrator

The orchestrator YAML sequences multiple subcommands into a named job, replacing a JCL job stream.

```bash
# 1. Copy and edit the example config
cp configs/orchestrator.yaml.example configs/orchestrator.yaml
vim configs/orchestrator.yaml   # adjust --input / --output paths

# 2. Run a named job
go run ./cmd/batch run --config configs/orchestrator.yaml --job daily-transaction-processing

# 3. Available predefined jobs in the example:
#    - daily-transaction-processing  (post-transaction, categorize-transaction, daily-rejects)
#    - monthly-statement             (interest-calc, create-statement, report-file)
```

### Add a custom job

Edit `configs/orchestrator.yaml`:

```yaml
jobs:
  my-job:
    description: "Custom job"
    steps:
      - name: refresh-accounts
        cmd: carddemo-batch account-file
        args: ["--input", "/data/ACCTDATA.PS"]
      - name: post-transactions
        cmd: carddemo-batch post-transaction
        args: ["--input", "/data/DALYTRAN.PS"]
```

Steps run in the order listed. A non-zero exit from any step halts the job.

---

## Restore application state

Because the user store is currently in-memory, "restoring state" means restarting the server (which re-seeds the default users) and re-importing flat files for batch data.

### Restore default users

```bash
# Just restart the server — default users are always re-seeded at startup.
go run ./cmd/carddemo
```

### Restore batch data files from mainframe export

If you have EBCDIC flat files from the mainframe (or from a previous export):

```bash
# Import user security
go run ./cmd/batch import --type usersec --input /backup/USRSEC.PS

# Import account data
go run ./cmd/batch import --type accounts --input /backup/ACCTDATA.PS

# Import all flat files at once (if supported by your import subcommand build)
go run ./cmd/batch import --all --input-dir /backup/
```

> **Password re-hash required on import.** `USRSEC.PS` files from the mainframe contain plaintext 8-character passwords in `SEC-USR-PWD`. The import subcommand must re-hash these with bcrypt before storing them. Check the import subcommand's `--rehash` flag.

### Reset to factory defaults

```bash
# 1. Stop the server
# 2. Restart — in-memory store is cleared and default users re-seeded
go run ./cmd/carddemo
```

---

## Common issues

### Login form returns 403 Forbidden

**Cause:** CSRF token is missing or invalid.

**Fix:** The CSRF double-submit pattern requires the browser to send both the `carddemo_csrf` cookie and either the `csrf_token` form field or `X-CSRF-Token` header. This happens automatically in normal browser use. If testing with `curl`, you must:

```bash
# 1. Fetch the login page and extract the CSRF token from the cookie
CSRF=$(curl -sc /tmp/cookies http://localhost:8080/login | grep -oP 'value="\K[^"]+' | head -1)

# 2. POST with the cookie jar and the token in the form body
curl -b /tmp/cookies -c /tmp/cookies \
  -d "user_id=ADMIN001&password=PASSWORD&csrf_token=${CSRF}" \
  http://localhost:8080/login
```

---

### Login fails with "too many attempts"

**Cause:** Rate limiter triggered — 5 failed attempts per user per 15-minute window, or 20 per IP.

**Fix:** Wait 15 minutes, or restart the server to clear the in-memory rate-limit counters.

---

### Session cookie not sent (login succeeds but subsequent requests redirect to /login)

**Cause:** `Secure` cookie flag set but you are using plain HTTP.

**Fix:**

```bash
CARDDEMO_INSECURE_COOKIES=1 go run ./cmd/carddemo
```

---

### "carddemo listening on :8080" then immediate exit

**Cause:** Port 8080 is already in use.

**Fix:**

```bash
lsof -i :8080          # find the process
CARDDEMO_ADDR=:9090 go run ./cmd/carddemo
```

---

### Batch subcommand exits with "not implemented"

**Cause:** Batch subcommands are stubs pending RAU-44.

**Fix:** Check the RAU-44 issue for the implementation timeline. The CLI scaffolding is in place; only the business logic is missing.

---

### EBCDIC flat file reads garbled data

**Cause:** File was FTP-transferred in ASCII (text) mode instead of binary mode.

**Fix:** Re-transfer the file in binary mode. All EBCDIC flat files must be transferred byte-for-byte without ASCII/EBCDIC translation — the Go `cobolfmt` package handles the EBCDIC-to-Unicode conversion internally.

---

### Decimal values show rounding errors

**Cause:** A code path is using `float64` for a monetary field instead of `decimal.Decimal`.

**Fix:** Check the code against ADR 0007. All `S9(n)V99` and `COMP-3` fields must use `decimal.Decimal`. Run `grep -rn 'float64' internal/domain internal/batch` to find violations.

---

## Log locations

The server writes structured logs to **stdout** using `log/slog`. There are no log files by default; redirect stdout to capture them.

```bash
# Capture to a file
go run ./cmd/carddemo 2>&1 | tee /tmp/carddemo.log

# Follow in real-time
go run ./cmd/carddemo 2>&1 | tee /dev/stderr | grep -v healthz
```

### Log fields

All log lines are JSON-structured when a JSON handler is configured. Default is text. Key fields:

| Field | Description |
|---|---|
| `time` | RFC3339 timestamp |
| `level` | `INFO`, `WARN`, `ERROR` |
| `msg` | Human-readable message |
| `addr` | Remote address (login/logout events) |
| `user_id` | Authenticated user, if known |
| `method` | HTTP method |
| `path` | HTTP path |

### Audit log

Login, logout, and user CRUD events are routed to `internal/audit.MemorySink` (in-memory, dev only). In production these should be directed to a persistent sink. The audit sink is separate from the application log and captures security-relevant events only.

```go
// To wire a persistent audit sink, replace audit.NewMemorySink() in cmd/carddemo/main.go:
sink := audit.NewMemorySink()     // dev
// sink := audit.NewFileSink("/var/log/carddemo-audit.jsonl")  // prod (once implemented)
```

---

## Makefile reference

```
make build     — compile all binaries (go build ./...)
make test      — run the full test suite
make lint      — run golangci-lint
make vet       — run go vet
make tidy      — go mod tidy
make run-web   — start cmd/web health-check stub on :8080 (pending RAU-43; use `go run ./cmd/carddemo` for login)
make run-batch — print cmd/batch usage
make clean     — remove build artefacts
make help      — list all targets
```
