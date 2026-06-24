// Package cobolfmt provides helpers for COBOL data-format conversions.
//
// COBOL programs use packed-decimal (COMP-3), zoned-decimal (DISPLAY NUMERIC),
// and fixed-width alphanumeric (PIC X) fields that have no direct Go equivalent.
// This package centralises those conversions so the domain and repo layers
// stay format-agnostic.
//
// Key conversions (to be implemented as porting progresses):
//
//	Comp3Decode(b []byte, scale int) decimal.Decimal   — packed-decimal → decimal
//	Comp3Encode(d decimal.Decimal, width int) []byte   — decimal → packed-decimal
//	ZonedDecode(b []byte, scale int) decimal.Decimal   — zoned-decimal → decimal
//	PadRight(s string, n int) string                   — PIC X(n) SPACE fill
//	TrimRight(s string) string                         — remove SPACE fill
//	ParseDate6(s string) (time.Time, error)            — YYMMDD → time.Time
//	ParseDate8(s string) (time.Time, error)            — YYYYMMDD → time.Time
//
// All monetary fields in the domain package use github.com/shopspring/decimal;
// float64 is never used for money (ADR 0007).
package cobolfmt
