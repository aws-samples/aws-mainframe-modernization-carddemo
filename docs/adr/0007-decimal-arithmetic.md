# ADR 0007 — Decimal arithmetic for COMP-3 and monetary fields

**Status:** Accepted (RAU-36)

## Context

COBOL COMP-3 (packed decimal) and DISPLAY NUMERIC fields represent monetary values
with exact decimal precision. IEEE 754 `float64` cannot represent all decimal fractions
exactly and must never be used for money (e.g. 0.1 + 0.2 ≠ 0.3 in floating point).

## Decision

**`github.com/shopspring/decimal` for all monetary and COMP-3 fields.**

- All domain fields representing money or count-with-scale use `decimal.Decimal`.
- `float64` is banned for monetary values; a `golangci-lint` custom rule enforces this.
- `internal/cobolfmt` provides `Comp3Decode` / `Comp3Encode` helpers that convert
  between packed-decimal bytes and `decimal.Decimal`.

## Alternatives considered

- `math/big.Float`: arbitrary precision but no fixed-scale semantics; error-prone for monetary rounding.
- `int64` cents: requires tracking scale separately; error-prone when mixing scales across programs.
- `shopspring/decimal`: widely used, supports COBOL-style rounding modes (ROUND_HALF_UP), serialises cleanly to JSON and SQL.

## Consequences

- All SQL columns for monetary fields use `NUMERIC(15,2)` (or equivalent).
- JSON serialisation uses `decimal.Decimal`'s `MarshalJSON` (decimal string, not float).
- Division and rounding use `decimal.Decimal.Div(...).Round(2)` — callers must specify scale explicitly.
