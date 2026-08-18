# Database

Schema migrations live with the backend (Flyway convention):
`backend/src/main/resources/db/migration/`.

- `V1__baseline_schema.sql` — all 13 tables per plan §2.2.

## Table inventory (plan §2.2)

| Table | Legacy source | PK | Notes |
|---|---|---|---|
| `accounts` | ACCTDAT (CVACT01Y) | `account_id` | `expiration_date` fixes the `ACCT-EXPIRAION-DATE` typo; `group_id` nullable, **no FK** (B1); `customer_id` FK from dissolved xref; `version` for JPA optimistic locking (R3) |
| `customers` | CUSTDAT (CVCUS01Y) | `customer_id` | `ssn` stored as loaded in Phase 0 (R5 open — encrypt before any API exposes it); `version` (R3) |
| `cards` | CARDDAT (CVACT02Y) | `card_number` | CVV **not persisted** (R5); FK → accounts; `idx_cards_account_id` replaces the CXACAIX AIX; `version` (R3) |
| *(dissolved)* | CXACAIX (CVACT03Y) | — | became `accounts.customer_id` + `cards.account_id`; ETL asserts 1:1 account→customer |
| `transactions` | TRANSACT (CVTRA05Y) | `transaction_id` | **no FK** on `(type_cd, category_cd)` (S5 — legacy posts unchecked); `idx_transactions_card_number` replaces TRANIDX AIX |
| `daily_transactions` | DALYTRAN (CVTRA06Y, sequential PS) | `daily_tran_id` | POSTTRAN staging; no card FK so reject reason 100 stays representable |
| `transaction_types` | TRANTYPE (CVTRA03Y) | `type_cd` | |
| `transaction_categories` | TRANCATG (CVTRA04Y) | `(type_cd, category_cd)` | FK → transaction_types |
| `disclosure_groups` | DISCGRP (CVTRA02Y) | `(group_id, type_cd, category_cd)` | includes DEFAULT rows for INTCALC fallback |
| `transaction_category_balances` | TCATBALF (CVTRA01Y) | `(account_id, type_cd, category_cd)` | FK → accounts |
| `users` | USRSEC (CSUSR01Y via DUSRSECJ.jcl) | `user_id` | `password_hash VARCHAR(72)` bcrypt; `user_type` CHECK IN ('A','U'); `version` (R3) |
| `transaction_rejects` | DALYREJS output (CBTRN02C) | `id BIGSERIAL` | **no CHECK** limiting `fail_reason` (109 exists; "ADD MORE VALIDATIONS HERE") |
| `statements` | CBSTM03A output (new) | `id BIGSERIAL` | indexes txt/html statement files |
| `report_runs` | CORPT00C/CBTRN03C output (new) | `id BIGSERIAL` | backs GET /reports + download |

## Type rules (plan §2.1)

Money → `NUMERIC` (never float); ids → `BIGINT`; `PIC X(10)` dates → `DATE`;
`PIC X(26)` DB2 timestamps → `TIMESTAMP`; text trimmed, all-blank → NULL;
1-char flags → `CHAR(1)` + CHECK.
