package cobolfmt

import (
	"fmt"
	"strings"

	"github.com/shopspring/decimal"
)

// DecodeZoned decodes an EBCDIC zoned-decimal field (DISPLAY NUMERIC in COBOL).
//
// In EBCDIC zoned decimal each byte stores one decimal digit:
//   - High nibble (zone): 0xF for unsigned digits; 0xC (positive) or 0xD
//     (negative) on the last byte of a signed field.
//   - Low nibble (digit): 0–9.
//
// Parameters:
//   - b: raw EBCDIC bytes of the field.
//   - scale: number of implied decimal digits (V in the PICTURE clause). A
//     field declared PIC S9(09)V99 has scale=2.
//   - signed: true when the field carries a sign (PIC S…).
func DecodeZoned(b []byte, scale int, signed bool) (decimal.Decimal, error) {
	if len(b) == 0 {
		return decimal.Zero, nil
	}

	digits := make([]byte, len(b))
	negative := false

	for i, byt := range b {
		hi := byt >> 4
		lo := byt & 0x0F

		isLast := i == len(b)-1
		if isLast && signed {
			switch hi {
			case 0xC, 0xF: // positive (0xF = unsigned, treat as positive)
			case 0xD:
				negative = true
			default:
				return decimal.Zero, fmt.Errorf("cobolfmt: zoned: invalid sign nibble 0x%X at byte %d", hi, i)
			}
		}

		if lo > 9 {
			return decimal.Zero, fmt.Errorf("cobolfmt: zoned: invalid digit nibble 0x%X at byte %d", lo, i)
		}
		digits[i] = '0' + lo
	}

	// Build a numeric string and parse.
	n := len(b)
	var s string
	switch {
	case scale <= 0:
		s = string(digits)
	case scale >= n:
		s = "0." + strings.Repeat("0", scale-n) + string(digits)
	default:
		s = string(digits[:n-scale]) + "." + string(digits[n-scale:])
	}

	d, err := decimal.NewFromString(s)
	if err != nil {
		return decimal.Zero, fmt.Errorf("cobolfmt: zoned: %w", err)
	}
	if negative {
		d = d.Neg()
	}
	return d, nil
}

// EncodeZoned encodes a decimal.Decimal into an EBCDIC zoned-decimal field.
//
// Parameters:
//   - d: value to encode.
//   - n: total number of digits (= byte width of the field).
//   - scale: implied decimal digits (V in PICTURE clause).
//   - signed: true when the field carries a sign (PIC S…).
//
// The encoded value is truncated / zero-padded on the left to exactly n digits.
// Callers are responsible for not passing a value that overflows the field width.
func EncodeZoned(d decimal.Decimal, n, scale int, signed bool) ([]byte, error) {
	if n <= 0 {
		return nil, fmt.Errorf("cobolfmt: zoned: field width must be > 0")
	}

	negative := d.IsNegative()
	if negative {
		d = d.Neg()
	}

	// Shift the decimal by scale places to work with an integer.
	shifted := d.Shift(int32(scale)) // #nosec G115 -- scale is always a small positive int within int32 range
	// Truncate any remaining fractional part.
	shifted = shifted.Floor()

	// Convert to a zero-padded digit string of exactly n chars.
	raw := shifted.String()
	// raw may contain a decimal point if scale==0 and d had fractional part;
	// strip any decimal portion.
	if idx := strings.IndexByte(raw, '.'); idx >= 0 {
		raw = raw[:idx]
	}
	if len(raw) < n {
		raw = strings.Repeat("0", n-len(raw)) + raw
	} else if len(raw) > n {
		raw = raw[len(raw)-n:] // left-truncate (overflow)
	}

	out := make([]byte, n)
	for i := 0; i < n; i++ {
		digit := raw[i] - '0'
		isLast := i == n-1
		if isLast && signed {
			if negative {
				out[i] = 0xD0 | digit // negative sign nibble
			} else {
				out[i] = 0xC0 | digit // positive sign nibble
			}
		} else {
			out[i] = 0xF0 | digit // unsigned zone nibble
		}
	}
	return out, nil
}

// DecodeZonedUint is a convenience wrapper that decodes an unsigned zoned field
// (PIC 9(n)) into an int64.
func DecodeZonedUint(b []byte) (int64, error) {
	d, err := DecodeZoned(b, 0, false)
	if err != nil {
		return 0, err
	}
	return d.IntPart(), nil
}

// EncodeZonedUint encodes a non-negative integer into a unsigned zoned field.
func EncodeZonedUint(v int64, n int) ([]byte, error) {
	return EncodeZoned(decimal.NewFromInt(v), n, 0, false)
}
