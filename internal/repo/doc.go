// Package repo provides repository interfaces and implementations for CardDemo persistence.
//
// VSAM KSDS files are replaced by a single relational backend:
// SQLite (modernc.org/sqlite, CGO-free) for development and integration tests,
// PostgreSQL (pgx/v5/stdlib) for production (ADR 0004).
//
// Callers program to interfaces only; no SQL is allowed outside this package.
//
// VSAM file → repository interface mapping:
//
//	USRSEC  (VSAM)  → UserSecRepository   (implemented: InMemoryUserSec)
//	ACCTDAT (VSAM)  → AccountRepository   (RAU-38)
//	CARDDAT (VSAM)  → CardRepository      (RAU-40)
//	CUSTDAT (VSAM)  → CustomerRepository  (RAU-37)
//	TRANSACT(VSAM)  → TransactionRepository (RAU-41)
//	CARDXREF(VSAM)  → CardXrefRepository  (RAU-40)
package repo
