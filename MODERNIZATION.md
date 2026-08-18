# CardDemo — 3-Tier Migration

Migration of the AWS CardDemo COBOL/CICS/VSAM sample to React + Spring Boot +
PostgreSQL, per the approved plan (`/code/.plans/v3-carddemo-migration.md`).
This repo currently contains **Phase 0 (Foundation)** — monorepo scaffold,
Flyway schema (13 tables), one-time ETL of the legacy sample data — and
**Phase 1 (Auth + read-only)** — JWT login, read-only REST API (accounts,
cards, transactions), and the React UI (login, dashboard, account view,
card list/detail, transaction list/detail) — and **Phase 2 (Updates + admin
CRUD)** — account update (COACTUPC rule engine), card update (COCRDUPC),
user admin CRUD (COUSR00C–03C), with optimistic locking and the matching
React edit screens — and **Phase 3 (Add transaction + bill payment)** —
COTRN02C add-transaction (full legacy edit suite, DB-sequence transaction
IDs) and COBIL00C full-balance bill payment (row-locked, exact legacy
transaction shape), with the two-step add form and checkbox-confirmed
bill-pay screens — and **Phase 4 (Batch: posting + interest)** — Spring
Batch `postTransactionsJob` (CBTRN02C POSTTRAN) and `interestCalcJob`
(CBACT04C INTCALC), admin trigger endpoints + `/admin/jobs` UI, and a
golden-file parity harness whose expected outputs are derived independently
of the Java (see `backend/src/test/resources/golden/derive_goldens.py`) —
and **Phase 5 (Reports + statements)** — `transactionReportJob` (CBTRN03C
TRANREPT, 133-char fixed-width report, requested via the CORPT00C-ported
`/reports` flow) and `statementJob` (CBSTM03A/B CREASTMT, per-account
plain-text + HTML statements), with report/statement download endpoints,
the `/reports` page, per-account statement lists, and their own
independently derived goldens
(`backend/src/test/resources/golden/derive_report_statement_goldens.py`).

## Layout (plan §1)

```
carddemo/
  frontend/    # Vite + React 18 + TypeScript (Router/TanStack Query/react-hook-form+zod installed; screens in Phase 1)
  backend/     # Spring Boot 3.5 / Java 21 (api + batch + etl profile), Flyway migrations
  db/          # schema documentation (migrations live in backend/src/main/resources/db/migration)
  etl/         # committed source fixtures (ASCII files + DUSRSECJ.jcl) + ETL docs
  docker-compose.yml
```

## Prerequisites

- JDK 21, Maven 3.8+
- Node 20+
- Docker (postgres:16-alpine)

## Quickstart (Phase 0 exit criteria)

```bash
# 1. start PostgreSQL 16
docker compose up -d postgres

# 2. run Flyway migrations + the one-time ETL (default: --seed-transactions ON)
cd backend
mvn spring-boot:run -Dspring-boot.run.profiles=etl

#    parity mode (empty transactions table, for Phase 4 fixtures):
#    mvn spring-boot:run -Dspring-boot.run.profiles=etl -Dspring-boot.run.arguments=--etl.seed-transactions=false
```

The ETL prints a row-count + checksum **validation report** and exits non-zero
(rolling back the load) if anything is RED. Expected populated state:

| Table | Rows |
|---|---|
| accounts / customers / cards | 50 each (all 50 `accounts.customer_id` backfilled from cardxref) |
| transaction_types / transaction_categories | 7 / 18 |
| disclosure_groups | 51 (incl. 17 DEFAULT rows) |
| transaction_category_balances | 50 |
| daily_transactions | 300 |
| transactions | 300 with the flag ON, 0 with it OFF |
| users | 10 (5 ADMIN + 5 USER, bcrypt `password_hash`; seed passwords are the legacy 8-char `PASSWORD` values from DUSRSECJ.jcl) |

Verify by hand:

```bash
docker exec carddemo-postgres psql -U carddemo -d carddemo \
  -c "SELECT (SELECT count(*) FROM accounts) accounts,
             (SELECT count(*) FROM transactions) transactions,
             (SELECT count(*) FROM users) users;"
```

## Running the app (Phase 1)

```bash
# 1. postgres seeded via the ETL (see Quickstart above)

# 2. backend API on :8080
cd backend
mvn spring-boot:run
#    JWT signing key and SSN encryption key default to dev-only values;
#    override BOTH in real envs (see Notes):
#    CARDDEMO_JWT_SECRET=<32+ byte secret> CARDDEMO_SSN_KEY=<key material> mvn spring-boot:run

# 3. frontend dev server on :5173 (proxies /api -> localhost:8080)
cd frontend
npm install
npm run dev
```

Sign in with the seeded legacy credentials (all 10 users share the password
`PASSWORD`, from DUSRSECJ.jcl):

| User ID | Role |
|---|---|
| `USER0001` … `USER0005` | USER |
| `ADMIN001` … `ADMIN005` | ADMIN (sees the Administration nav; admin screens land in a later phase) |

API endpoints (JWT `Authorization: Bearer` required except login):

- `POST /api/v1/auth/login` → `{token, expiresInSeconds, userId, firstName, lastName, role}`
- `GET /api/v1/accounts/{id}` — account + customer view (SSN masked to last 4)
- `GET /api/v1/cards?accountId=&cursor=&limit=` — keyset-paginated, numbers masked
- `GET /api/v1/cards/{cardNumber}` — full card detail
- `GET /api/v1/transactions?fromId=&cursor=&limit=` and `GET /api/v1/transactions/{id}`

Phase 2 (updates + admin CRUD; admin endpoints require the ADMIN role):

- `PUT /api/v1/accounts/{id}` — account + customer update, validated by the
  COACTUPC rule engine (`validation/AccountUpdateValidator`); optimistic
  locking via `accountVersion`/`customerVersion` (409 on stale, mirroring
  the legacy "Record changed by some one else" flow); `ssn: null` keeps the
  stored value (the API only ever returns the last 4)
- `PUT /api/v1/cards/{cardNumber}` — embossed name / expiry / active status
  (COCRDUPC edits), optimistic locking via `version`
- `GET /api/v1/users?cursor=&limit=` — keyset-paginated user list (COUSR00C)
- `POST /api/v1/users` — add user (COUSR01C; duplicate ID → 409)
- `GET/PUT/DELETE /api/v1/users/{id}` — view/update/delete (COUSR02C/03C;
  `password: null` on update keeps the current one)

Phase 3 (add transaction + bill payment; both USER-accessible):

- `POST /api/v1/transactions` — COTRN02C add. Body: all-string fields
  `{accountId | cardNumber, typeCd, categoryCd, source, description, amount,
  origDate, procDate, merchantId, merchantName, merchantCity, merchantZip}`.
  The server re-runs the legacy edit suite fail-fast (first failed edit is
  the single `fieldError`, exact legacy messages); `amount` must be in the
  exact legacy format `[+-]\d{8}\.\d{2}` (the UI normalizes e.g. `75.00` →
  `+00000075.00`); orig/proc timestamps come from the user-entered dates
  (COTRN02C 464-465). Transaction IDs come from a DB sequence rendered as a
  16-digit zero-padded string (plan R2 — replaces the race-prone READPREV
  max+1). Returns 201 with the created transaction view.
- `POST /api/v1/accounts/{id}/bill-payment` — COBIL00C. Body
  `{confirm: true}`. Locks the account row (`SELECT ... FOR UPDATE`, plan
  R3), rejects a `<= 0` balance with the legacy "You have nothing to pay..."
  message, writes the exact legacy-shaped payment transaction (type `02`,
  category 2, source `POS TERM`, description `BILL PAYMENT - ONLINE`,
  merchant `999999999`/`BILL PAYMENT`/city+zip `N/A`, amount = full
  balance), and zeroes `curr_bal` — all in one DB transaction. Returns 201.

Phase 4 (batch; ADMIN-only):

- `POST /api/v1/admin/jobs/postTransactions/run` — runs POSTTRAN (CBTRN02C)
  synchronously: reads `daily_transactions` in id order, validates each
  record (last-wins reject semantics: 100 bad card short-circuits; otherwise
  101 missing account; otherwise the 102 overlimit and 103 expired checks
  BOTH run and 103 overwrites 102), writes failures to `transaction_rejects`
  (tagged with the job execution id) and posts the rest: category-balance
  upsert, account `curr_bal`/cycle update, `transactions` insert keeping
  `transaction_id = daily_tran_id`. Exit code `COMPLETED_WITH_REJECTS` when
  any record was rejected (the legacy RETURN-CODE 4 analogue). Returns 201
  with the run view `{runId, jobName, status, exitCode, startTime, endTime,
  readCount, writeCount, posted, rejected}`.
- `POST /api/v1/admin/jobs/interestCalc/run` — runs INTCALC (CBACT04C): one
  interest transaction per `transaction_category_balances` row
  (`bal * rate / 1200` truncated toward zero — the COBOL COMPUTE has no
  ROUNDED clause; disclosure-group lookup with `DEFAULT` fallback; a missing
  DEFAULT row FAILS the job, matching the legacy abend), account totals
  applied with a cycle reset at each account break. The legacy last-account
  quirk (final account never flushed — dead code in the CBACT04C main loop)
  is reproduced deliberately.
- `GET /api/v1/admin/jobs/runs` — run history from Spring Batch's own
  `BATCH_*` metadata tables (the GDG replacement, Flyway-managed in
  `V3__spring_batch_metadata.sql`).
- Frontend: `/admin/jobs` — run buttons + run-history table.
- Jobs never run on boot (`spring.batch.job.enabled: false`); POSTTRAN
  re-runs post already-posted ids into `transactions` and will fail on the
  duplicate key — reset the seed between runs (parity tests always start
  from a fresh ETL load).

Phase 5 (reports + statements):

- `POST /api/v1/reports/transaction` — CORPT00C: body
  `{reportType: "monthly"|"yearly"|"custom", startMonth, startDay, startYear,
  endMonth, endDay, endYear}` (per-part date strings, exactly the BMS
  fields). Monthly = first..last day of the current month; yearly = Jan 1..
  Dec 31; custom runs the full legacy edit chain (per-part empty checks,
  NUMVAL-C normalization, month/day range edits, whole-date CSUTLDTC edit)
  and reports the FIRST failing edit with the exact legacy message. On
  success it inserts a `report_runs` row and synchronously runs
  `transactionReportJob` (CBTRN03C): posted transactions with
  `proc_ts::date` in range, sorted by (card, transaction id) — the
  TRANREPT.prc STEP05R SORT — rendered as 133-char lines with page totals
  every 20 counted lines, per-card "account" totals, and a grand total
  (including the legacy EOF quirk: the last record's amount is re-added and
  the last card's account total is dropped). Any authenticated user (legacy
  CORPT00 is a usrtype 'U' menu entry). Returns 201 with the run view and
  the legacy `... report submitted for printing ...` message.
- `GET /api/v1/reports` — run history (`report_runs`, newest first).
- `GET /api/v1/reports/{id}/download` — the report file as a `text/plain`
  attachment; 409 while the run is not COMPLETED.
- `POST /api/v1/admin/jobs/statements/run` (ADMIN) — runs CREASTMT
  (CBSTM03A/B): one plain-text (80-char) + HTML (100-char) statement per
  card-xref record in card-number order, transactions per card in
  transaction-id order (the CREASTMT.JCL SORT), Total EXP = sum of the
  card's transaction amounts, indexed by a `statements` row per statement.
  Optional job parameter `cycleDate` (defaults to today).
- `GET /api/v1/accounts/{id}/statements` — statement index for an account.
- `GET /api/v1/statements/{id}?format=txt|html` — txt as an attachment,
  html inline.
- Batch files are written under `carddemo.files.dir`
  (`CARDDEMO_FILES_DIR`, default `./data/files`): `reports/` and
  `statements/`. Note: `transactionReportJob` is NOT admin-triggerable via
  `/admin/jobs` — it is launched only through `POST /reports/transaction`,
  which owns the `report_runs` bookkeeping.
- Frontend: `/reports` (request form + past-runs table with downloads, the
  legacy Y/N confirm as a browser confirm dialog), a Statements card on the
  account detail page (txt download / HTML view via authed blob fetches),
  and a CREASTMT card on `/admin/jobs`.

Validation failures return 400 with
`{error:"validation", message, fieldErrors:[{field,message}]}`; stale-version
conflicts return 409 `{error:"conflict", message}`.

Frontend routes: `/accounts/:id/edit`, `/cards/:cardNumber/edit`,
`/admin/users`, `/admin/users/:userId/edit`, `/transactions/new`,
`/accounts/:id/bill-pay`.

OpenAPI UI at `http://localhost:8080/swagger-ui/index.html`.

## Tests

```bash
# backend: parser + ETL end-to-end (Testcontainers, needs Docker) + API/security
# integration tests + Phase 4 batch golden-file parity (seeds parity mode, runs
# both jobs, diffs transactions/rejects/accounts/tcatbal against goldens derived
# independently by golden/derive_goldens.py; synthetic fixtures cover reject
# reasons 100/101 and the 103-overwrites-102 case; missing-DEFAULT aborts)
# + Phase 5 parity (POSTTRAN -> CBTRN03C report diffed line-by-line vs the
# derived golden; CBSTM03A statements for accounts 50/27 diffed vs goldens +
# HTML structural checks) and the CORPT00C date-validation matrix
cd backend && mvn test

# regenerate the goldens (only needed if the ASCII fixtures ever change):
python3 backend/src/test/resources/golden/derive_goldens.py
# Phase 5: report + statement goldens (hand-executed CBTRN03C/CBSTM03A rules):
python3 backend/src/test/resources/golden/derive_report_statement_goldens.py

# frontend: unit tests (Vitest), lint, type-check + production build
cd frontend && npm install && npm test && npm run lint && npm run build
```

## Notes

- **Production secrets**: the demo defaults in `application.yml` are for local
  use only. Production MUST set `CARDDEMO_JWT_SECRET` (>=32 bytes, HS256 JWT
  signing) and `CARDDEMO_SSN_KEY` (AES-256-GCM key material for `customers.ssn`
  at rest).
- **SSN at rest (V4)**: `customers.ssn` stores app-level AES-GCM ciphertext
  (JPA converter `crypto/SsnAttributeConverter`; the ETL encrypts on load).
  The V4 migration NULLs any pre-V4 plaintext rows — after upgrading an
  existing database, re-run the ETL to reload encrypted values. Rotating
  `CARDDEMO_SSN_KEY` likewise requires an ETL re-run (ciphertexts are not
  re-encrypted in place). This supersedes the stale "stored as loaded" NOTE in
  `V1__baseline_schema.sql` (V1 is checksum-locked and cannot be edited).
- The ETL is idempotent only against an empty schema; to re-run, drop the
  volume first: `docker compose down -v && docker compose up -d postgres`.
- `--etl.seed-transactions` (plan §2.3): the 300 `dailytran.txt` records are
  also pre-loaded into `transactions` for demo browsing. Phase 4 POSTTRAN
  parity seeding must run with the flag OFF.
- Deliberate schema decisions (no FK on `accounts.group_id`, no FK on
  `transactions(type_cd,category_cd)`, no CHECK on reject reasons) are
  documented in `db/README.md` and in `V1__baseline_schema.sql` comments.
