# ETL — one-time load of the legacy sample data (plan §2.3)

The loader is implemented as a Spring profile inside the backend
(`backend/src/main/java/com/carddemo/etl/`), activated with
`--spring.profiles.active=etl`. This directory holds the committed source
fixtures and documentation.

## Fixtures

- `fixtures/ascii/*.txt` — verbatim copies of
  `aws-mainframe-modernization-carddemo/app/data/ASCII/` (fixed-width ASCII,
  zoned-decimal overpunch signs).
- `fixtures/jcl/DUSRSECJ.jcl` — the 10 seed users exist **only** as in-stream
  SYSUT1 records in this JCL (80-byte records per `CSUSR01Y`); there is no
  `app/data/usrsec` file.

| File | Records | Copybook | Target table |
|---|---|---|---|
| trantype.txt | 7 | CVTRA03Y | transaction_types |
| trancatg.txt | 18 | CVTRA04Y | transaction_categories |
| discgrp.txt | 51 (17 DEFAULT) | CVTRA02Y | disclosure_groups |
| custdata.txt | 50 | CVCUS01Y | customers |
| acctdata.txt | 50 | CVACT01Y | accounts |
| carddata.txt | 50 | CVACT02Y | cards |
| cardxref.txt | 50 | CVACT03Y | *(dissolved — backfills accounts.customer_id)* |
| tcatbal.txt | 50 | CVTRA01Y | transaction_category_balances |
| dailytran.txt | 300 | CVTRA06Y | daily_transactions (+ transactions, see flag) |
| DUSRSECJ.jcl in-stream | 10 | CSUSR01Y | users (passwords bcrypt-hashed) |

## Rules implemented

- **Fixed-width parsing from copybook offsets** — see the offset tables in the
  record classes' javadoc; never guessed from data.
- **Zoned-decimal sign decoding**: `{`=+0, `A`–`I`=+1..9, `}`=-0, `J`–`R`=-1..-9
  (`ZonedDecimal.java`).
- **Trim + blank→NULL** for text; strict ISO date / DB2 timestamp parsing
  (unparseable aborts the load).
- **Blank `ACCT-GROUP-ID` → NULL** (plan B1; all 50 sample accounts are blank).
- **Xref 1:1 assertion (N8)**: any account mapping to >1 distinct customer
  aborts loudly before anything is written.
- **FK violations reported, never silently dropped** — each violation is listed
  in the validation report, which turns RED and rolls back the load.
- **Row-count + checksum validation report** printed at the end (counts per
  table plus NUMERIC sums for balances/limits/amounts/rates).
- **`--etl.seed-transactions` flag** (default `true`): pre-loads the 300
  dailytran records into `transactions` for Phase 1 browsing. This is a
  deliberate divergence — legacy TRANSACT starts near-empty. Set `false` for
  Phase 4 POSTTRAN parity fixtures (posted inserts keep
  `TRAN-ID = DALYTRAN-ID` and would collide with the pre-load).
- **Load order**: trantype → trancatg → discgrp → customers → accounts → cards
  → xref FK backfill → tcatbal → daily_transactions → transactions (flag) →
  users.

## Running

See the root `README.md` for the exact commands.
