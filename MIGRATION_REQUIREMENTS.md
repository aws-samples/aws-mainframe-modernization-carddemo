# CardDemo Migration — Requirements & Test Cases Document

**File:** `MIGRATION_REQUIREMENTS.md`
**Project:** Migration of `aws-mainframe-modernization-carddemo` from COBOL/CICS/VSAM to React.js (frontend) + Spring Boot (backend)
**Version:** 1.0

---

## 1. Purpose

This document defines the functional requirements, traceability mapping, functional test cases, and scoring criteria for converting the CardDemo mainframe application into a two-tier web application:

- **Frontend:** React.js (Vite, react-router)
- **Backend:** Spring Boot 3 (Java 17, Maven), Spring Data JPA, Spring Security, Spring Batch
- **Database:** H2 (dev) / PostgreSQL (prod)

## 2. Scope

### 2.1 In Scope

| Artifact | Location | Count |
|---|---|---|
| Online COBOL programs | `app/cbl/CO*.cbl` | 17 |
| Batch COBOL programs | `app/cbl/CB*.cbl` | 10 |
| Date utility | `app/cbl/CSUTLDTC.cbl` | 1 |
| BMS screen maps | `app/bms/` | 17 maps |
| VSAM record copybooks | `app/cpy/` | ~20 copybooks |
| Seed data | `app/data/ASCII/` | all files |

### 2.2 Out of Scope

- Optional extension modules: `app-authorization-ims-db2-mq/`, `app-transaction-type-db2/`, `app-vsam-mq/`
- Mainframe infrastructure: `app/asm/`, `app/jcl/`, `app/proc/`, `app/scheduler/`, `app/csd/`
- `CBEXPORT.cbl`, `CBIMPORT.cbl`, `CVEXPORT.cpy` (mainframe data interchange — replaced by SQL seeding)
- `COBSWAIT.cbl` (batch scheduling shim — replaced by Spring scheduling)
- `UNUSED1Y.cpy`

## 3. Functional Requirements

### 3.1 Authentication & Navigation

| REQ-ID | Requirement | Source |
|---|---|---|
| REQ-AUTH-01 | Login screen validates userId + password against user security table. Wrong credentials → error message and remain on login. Source: `COSGN00C.cbl`, `COSGN00.bms`, `CSUSR01Y.cpy` | |
| REQ-AUTH-02 | Role-based routing: `ADMIN` type users land on admin menu (`COADM01`); `USER` type on regular menu (`COMEN01`). Admin endpoints/pages must reject USER role. Source: `COADM01C.cbl`, `COMEN01C.cbl` | |
| REQ-AUTH-03 | Unauthenticated access to any page/API redirects to login / returns 401. | |

### 3.2 Accounts

| REQ-ID | Requirement | Source |
|---|---|---|
| REQ-ACCT-01 | Account View: lookup by 11-digit account ID; display all fields of `CVACT01Y.cpy`; invalid/not-found ID → COBOL error message. Source: `COACTVWC.cbl`, `COACTVW.bms` | |
| REQ-ACCT-02 | Account Update: modify account fields with the same field-level validations as `COACTUPC.cbl` (required fields, lengths, date formats, numeric ranges). Source: `COACTUPC.cbl`, `COACTUP.bms` | |

### 3.3 Cards

| REQ-ID | Requirement | Source |
|---|---|---|
| REQ-CARD-01 | Card List: paginated listing of cards (PF7/PF8 → prev/next buttons). Source: `COCRDLIC.cbl`, `COCRDLI.bms` | |
| REQ-CARD-02 | Card Detail: view a single card by card number; fields from `CVACT02Y.cpy`. Source: `COCRDSLC.cbl`, `COCRDSL.bms` | |
| REQ-CARD-03 | Card Update: edit card fields with COBOL-equivalent validation. Source: `COCRDUPC.cbl`, `COCRDUP.bms` | |

### 3.4 Transactions

| REQ-ID | Requirement | Source |
|---|---|---|
| REQ-TRXN-01 | Transaction List: paginated list of transactions (`CVTRA05Y.cpy` record). Source: `COTRN00C.cbl`, `COTRN00.bms` | |
| REQ-TRXN-02 | Transaction Detail: view single transaction. Source: `COTRN01C.cbl`, `COTRN01.bms` | |
| REQ-TRXN-03 | Transaction Add: create a transaction; validate card number, amount, type, and date via ported `CSUTLDTC.cbl` date-validation logic. Source: `COTRN02C.cbl`, `COTRN02.bms` | |
| REQ-RPT-01 | Transaction Report: submit date range and display transactions in range. Source: `CORPT00C.cbl`, `CORPT00.bms` | |

### 3.5 Bill Payment

| REQ-ID | Requirement | Source |
|---|---|---|
| REQ-BILL-01 | Bill Pay: look up account, confirm balance, post payment → creates a payment transaction record and updates account balance atomically. Zero/invalid balance handled per COBOL logic. Source: `COBIL00C.cbl`, `COBIL00.bms` | |

### 3.6 User Administration (ADMIN only)

| REQ-ID | Requirement | Source |
|---|---|---|
| REQ-USR-01 | User List: paginated display of `CSUSR01Y` records. Source: `COUSR00C.cbl`, `COUSR00.bms` | |
| REQ-USR-02 | User Add: create user with ID, name, password, type (A/U); duplicate-ID rejection. Source: `COUSR01C.cbl`, `COUSR01.bms` | |
| REQ-USR-03 | User Update: modify user fields. Source: `COUSR02C.cbl`, `COUSR02.bms` | |
| REQ-USR-04 | User Delete: remove user with confirmation. Source: `COUSR03C.cbl`, `COUSR03.bms` | |

### 3.7 Data Model & Seed Data

| REQ-ID | Requirement | Source |
|---|---|---|
| REQ-DATA-01 | JPA entities + repositories for: Account (`CVACT01Y`), Card (`CVACT02Y`), CardXref (`CVACT03Y`), Customer (`CVCUS01Y`/`CUSTREC`), Transaction (`CVTRA05Y`/`CVTRA06Y`), DailyTransaction (`CVTRA07Y`), TranCatBal (`CVTRA01Y`), DisclosureGroup (`CVTRA02Y`), TransactionType (`CVTRA03Y`), TransactionCategory (`CVTRA04Y`), CardXrefRecord (`CVCRD01Y`), UserSecurity (`CSUSR01Y`) | |
| REQ-DATA-02 | All tables seeded from `app/data/ASCII/` files with 100% row-count and key-field parity | |

### 3.8 Batch Jobs

| REQ-ID | Requirement | Source |
|---|---|---|
| REQ-BATCH-01 | Transaction posting job: read daily transactions → post to transaction file + update balances. Source: `CBTRN01C`, `CBTRN02C`, `CBTRN03C` | |
| REQ-BATCH-02 | Interest calculation job: monthly interest on account balances using fixed-point math identical to `CBACT04C.cbl` | |
| REQ-BATCH-03 | Statement generation job: statement file output per account. Source: `CBSTM03A.CBL`, `CBSTM03B.CBL`, `COSTM01.CPY` | |
| REQ-BATCH-04 | Account/card/customer file-load jobs replaced by DB seeding or import endpoints. Source: `CBACT01C`, `CBACT02C`, `CBACT03C`, `CBCUS01C` | |

### 3.9 Non-Functional Requirements

| REQ-ID | Requirement |
|---|---|
| REQ-NF-01 | All money/interest math uses `BigDecimal` reproducing COBOL COMP-3 packed-decimal precision and rounding exactly |
| REQ-NF-02 | UI error messages match COBOL `WS-ERR-MSG`/`CSMSG01Y`/`CSMSG02Y` message text |
| REQ-NF-03 | PF-key behaviors mapped to UI equivalents (F3 → Back, F7/PF7 → Prev page, PF8 → Next page, F12 → Exit/Logout) |
| REQ-NF-04 | API responses return proper HTTP codes (200/201/400/401/403/404) — no 500s for business-rule failures |
| REQ-NF-05 | Field masks/lengths enforced identically to BMS map attributes |

## 4. Traceability Map

| COBOL artifact | React page | REST endpoint | Entity/Table |
|---|---|---|---|
| `COSGN00C` / `COSGN00.bms` | `LoginPage` | `POST /api/auth/login` | `UserSecurity` |
| `COMEN01C` / `COMEN01.bms` | `MainMenuPage` | `GET /api/menu` | — |
| `COADM01C` / `COADM01.bms` | `AdminMenuPage` | — | — |
| `COACTVWC` / `COACTVW.bms` | `AccountViewPage` | `GET /api/accounts/{id}` | `Account` |
| `COACTUPC` / `COACTUP.bms` | `AccountUpdatePage` | `PUT /api/accounts/{id}` | `Account` |
| `COCRDLIC` / `COCRDLI.bms` | `CardListPage` | `GET /api/cards?page=` | `Card` |
| `COCRDSLC` / `COCRDSL.bms` | `CardDetailPage` | `GET /api/cards/{num}` | `Card` |
| `COCRDUPC` / `COCRDUP.bms` | `CardUpdatePage` | `PUT /api/cards/{num}` | `Card` |
| `COTRN00C` / `COTRN00.bms` | `TransactionListPage` | `GET /api/transactions?page=` | `Transaction` |
| `COTRN01C` / `COTRN01.bms` | `TransactionViewPage` | `GET /api/transactions/{id}` | `Transaction` |
| `COTRN02C` / `COTRN02.bms` | `TransactionAddPage` | `POST /api/transactions` | `Transaction` |
| `CORPT00C` / `CORPT00.bms` | `ReportPage` | `GET /api/reports/transactions?from=&to=` | `Transaction` |
| `COBIL00C` / `COBIL00.bms` | `BillPayPage` | `POST /api/accounts/{id}/payment` | `Account`, `Transaction` |
| `COUSR00C`–`03C` / `COUSR00`–`03.bms` | `UserList/Add/Update/DeletePage` | `GET/POST/PUT/DELETE /api/admin/users` | `UserSecurity` |
| `CBTRN01C`–`03C` | — | `POST /api/jobs/posting` | `Transaction`, `Account` |
| `CBACT04C` | — | `POST /api/jobs/interest` | `Account` |
| `CBSTM03A/B` | — | `POST /api/jobs/statements` | `Statement` |
| `CSUTLDTC.cbl` | — | `DateValidator` utility (Java `LocalDate`) | — |

## 5. Functional Test Cases

Pass criteria reference seeded data from `app/data/ASCII/`. Default credentials: `ADMIN001` / `PASSWORD` (admin), `USER0001` / `PASSWORD` (user) — confirm against actual seed file.

### 5.1 Authentication & Navigation

| TC-ID | REQ | Steps | Expected |
|---|---|---|---|
| TC-01 | AUTH-01 | Login `ADMIN001`/`PASSWORD` | Login succeeds → admin menu displayed |
| TC-02 | AUTH-01 | Login `USER0001`/`PASSWORD` | Login succeeds → regular menu; no admin options |
| TC-03 | AUTH-01 | Login with wrong password and with unknown userId | COSGN00C error text shown; remains on login |
| TC-04 | AUTH-02 | As USER, call `GET /api/admin/users` directly | HTTP 403 |
| TC-05 | AUTH-03 | Open `/accounts` URL and call `GET /api/accounts` with no token | Redirect to login / HTTP 401 |
| TC-06 | AUTH-01 | Logout → attempt back-navigation | Session cleared; redirected to login |

### 5.2 Accounts

| TC-ID | REQ | Steps | Expected |
|---|---|---|---|
| TC-07 | ACCT-01 | Account View with a valid seeded account ID | All fields render and match the `CVACT01Y` record exactly |
| TC-08 | ACCT-01 | Enter non-existent ID; enter non-numeric/short ID | "Account not found" / field-format errors matching COBOL messages |
| TC-09 | ACCT-02 | Update editable account fields with valid values | 200; re-view shows persisted values |
| TC-10 | ACCT-02 | Submit invalid values (bad date, wrong length, empty required) | Rejected with same validation errors as `COACTUPC` |

### 5.3 Cards

| TC-ID | REQ | Steps | Expected |
|---|---|---|---|
| TC-11 | CARD-01 | Open Card List; page through next/prev to last page | Correct page contents; next disabled on last page (PF8/PF7 parity) |
| TC-12 | CARD-02 | Open card detail by seeded card number | Detail matches `CVACT02Y` record |
| TC-13 | CARD-03 | Update card with valid data, then invalid expiry/status | Valid save succeeds; invalid rejected with COBOL error text |

### 5.4 Transactions & Reports

| TC-ID | REQ | Steps | Expected |
|---|---|---|---|
| TC-14 | TRXN-01 | Transaction List with pagination | Paginated `CVTRA05Y` records; prev/next correct |
| TC-15 | TRXN-02 | Open transaction detail | All fields match record |
| TC-16 | TRXN-03 | Add valid transaction | Persisted; visible in list and detail |
| TC-17 | TRXN-03 | Add transaction with invalid dates (month=13, day=40, Feb 30, non-leap Feb 29) | All rejected — `CSUTLDTC` parity |
| TC-18 | TRXN-03 | Add with invalid amount, unknown card, bad type/category | Field-level errors shown; nothing persisted |
| TC-19 | RPT-01 | Report over a seeded date range; then over an empty range | Correct transactions listed / "no records" message |
| TC-20 | RPT-01 | Submit from-date > to-date | Validation error |

### 5.5 Bill Payment

| TC-ID | REQ | Steps | Expected |
|---|---|---|---|
| TC-21 | BILL-01 | Pay full balance on seeded account | Payment transaction created; account balance updated; both visible in UI/API |
| TC-22 | BILL-01 | Pay on zero-balance account; enter invalid account | Rejected per `COBIL00C` logic |

### 5.6 User Administration

| TC-ID | REQ | Steps | Expected |
|---|---|---|---|
| TC-23 | USR-01 | As ADMIN, list users with pagination | All `CSUSR01Y` users listed |
| TC-24 | USR-02 | Add new user; then attempt duplicate ID | User created & can log in; duplicate rejected |
| TC-25 | USR-03 | Update a user's name/password/type | Persisted; new password/type effective at next login |
| TC-26 | USR-04 | Delete a test user; verify login fails afterward | User removed; login denied |
| TC-27 | USR-01..04 | As USER, attempt any user-management action | All denied (403 + hidden UI) |

### 5.7 Data, Batch & Non-Functional

| TC-ID | REQ | Steps | Expected |
|---|---|---|---|
| TC-28 | DATA-01/02 | Compare row counts + key fields of every table vs `app/data/ASCII/` | 100% match |
| TC-29 | BATCH-01 | Run posting job on seeded daily transactions | Balances/transactions updated per `CBTRN02C` rules |
| TC-30 | BATCH-02 | Run interest job; hand-compute expected interest | Matches to the cent (COMP-3 parity) |
| TC-31 | BATCH-03 | Run statement job | Statement contains correct transactions and totals |
| TC-32 | NF-01/02 | Trigger each COBOL validation; compare error text | Messages match `CSMSG01Y`/`WS-ERR-MSG` |
| TC-33 | NF-03 | Exercise all PF-key equivalents (F3 back, F7/F8 paging, F12 logout) | Behavior consistent across all pages |
| TC-34 | ALL | End-to-end: login → menu → account → card → transaction add → bill pay → report → logout | Entire flow completes; navigation mirrors menu structure |

## 6. Scoring

- **Overall score** = (passed TCs / 34) × 100%.
- A **requirement is satisfied only if every linked TC passes**.
- **Sign-off threshold:** overall ≥ 90% **and** TC-21, TC-29, TC-30 must all pass (financial correctness is mandatory).

### Results Table (fill during testing)

| Area | REQ-IDs | TCs | Passed | Satisfied? |
|---|---|---|---|---|
| Auth & navigation | AUTH-01..03 | TC-01..06 | __/6 | |
| Accounts | ACCT-01..02 | TC-07..10 | __/4 | |
| Cards | CARD-01..03 | TC-11..13 | __/3 | |
| Transactions & reports | TRXN-01..03, RPT-01 | TC-14..20 | __/7 | |
| Bill payment | BILL-01 | TC-21..22 | __/2 | |
| User admin | USR-01..04 | TC-23..27 | __/5 | |
| Data & batch | DATA-01..02, BATCH-01..04 | TC-28..31 | __/4 | |
| Non-functional & e2e | NF-01..05 | TC-32..34 | __/3 | |
| **Total** | | **34** | **__/34** | |

**Final migration score: ____ %** ( ≥ 90% with all financial TCs passing = migration accepted )
