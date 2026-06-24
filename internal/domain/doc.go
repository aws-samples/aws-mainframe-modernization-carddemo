// Package domain holds the CardDemo domain types ported from COBOL copybooks.
//
// Each struct maps to a COBOL data-layout (copybook or WORKING-STORAGE section)
// and carries the same field semantics with Go-idiomatic naming.
//
// COBOL copybook → Go struct mapping:
//
//	CSUSR01Y.cpy   → UserSec     (user security record, 80 bytes)
//	CVACT01Y.cpy   → Account     (account master, RAU-38)
//	CVACT02Y.cpy   → AccountView (account view, RAU-38)
//	CVACT03Y.cpy   → AccountXref (account cross-reference, RAU-38)
//	CVCRD01Y.cpy   → CreditCard  (credit-card record, RAU-40)
//	CVCUS01Y.cpy   → Customer    (customer master, RAU-37)
//	CVTRA05Y.cpy   → Transaction (transaction record, RAU-41)
//
// All monetary fields use github.com/shopspring/decimal (ADR 0007); float64 is banned.
// Timestamps are stored as Unix seconds (int64) to match the SQLite schema.
package domain
