package cobolfmt

import (
	"fmt"
	"strings"

	"github.com/shopspring/decimal"
)

// PackedSize returns the storage size in bytes for a COMP-3 (packed decimal) field
// with the given total number of digits (integer + decimal).
//
// IBM COBOL packs each digit into one nibble; the last nibble is the sign.
// Storage = ⌈(digits + 1) / 2⌉.
func PackedSize(digits int) int {
	return (digits + 2) / 2 // equivalent to ceil((digits+1)/2)
}

// DecodePacked decodes a COMP-3 packed-decimal field.
//
// Layout: each byte holds two BCD digits in its high and low nibbles.
// The very last nibble is the sign: 0xC or 0xF = positive, 0xD = negative.
// The first nibble of the first byte may be 0 (padding for even-digit fields).
//
// Parameters:
//   - b: raw bytes of the COMP-3 field.
//   - scale: implied decimal digits (V in PICTURE clause).
func DecodePacked(b []byte, scale int) (decimal.Decimal, error) {
	if len(b) == 0 {
		return decimal.Zero, nil
	}

	// Flatten nibbles; last nibble is the sign.
	totalNibbles := len(b) * 2
	nibbles := make([]byte, totalNibbles)
	for i, byt := range b {
		nibbles[i*2] = byt >> 4
		nibbles[i*2+1] = byt & 0x0F
	}

	signNibble := nibbles[len(nibbles)-1]
	negative := false
	switch signNibble {
	case 0xC, 0xF: // positive / unsigned
	case 0xD:
		negative = true
	default:
		return decimal.Zero, fmt.Errorf("cobolfmt: packed: invalid sign nibble 0x%X", signNibble)
	}

	// Digit nibbles (everything except the last nibble).
	digitNibbles := nibbles[:len(nibbles)-1]
	digits := make([]byte, 0, len(digitNibbles))
	leadingZero := true
	for _, n := range digitNibbles {
		if n > 9 {
			return decimal.Zero, fmt.Errorf("cobolfmt: packed: invalid digit nibble 0x%X", n)
		}
		if leadingZero && n == 0 {
			continue // skip leading zeros for the integer string
		}
		leadingZero = false
		digits = append(digits, '0'+n)
	}

	// Build integer string including leading zeros needed for scale.
	raw := string(digits)
	if raw == "" {
		raw = "0"
	}

	// Ensure there are enough characters for the scale.
	if scale > 0 && len(raw) <= scale {
		raw = strings.Repeat("0", scale+1-len(raw)) + raw
	}

	var s string
	switch {
	case scale <= 0:
		s = raw
	case scale >= len(raw):
		s = "0." + strings.Repeat("0", scale-len(raw)) + raw
	default:
		s = raw[:len(raw)-scale] + "." + raw[len(raw)-scale:]
	}

	d, err := decimal.NewFromString(s)
	if err != nil {
		return decimal.Zero, fmt.Errorf("cobolfmt: packed: %w", err)
	}
	if negative {
		d = d.Neg()
	}
	return d, nil
}

// EncodePacked encodes a decimal.Decimal into a COMP-3 packed-decimal field.
//
// Parameters:
//   - d: value to encode.
//   - digits: total number of BCD digits in the field (integer + decimal).
//   - scale: implied decimal digits (V in PICTURE clause).
//
// The encoded byte slice has length PackedSize(digits).
func EncodePacked(d decimal.Decimal, digits, scale int) ([]byte, error) {
	if digits <= 0 {
		return nil, fmt.Errorf("cobolfmt: packed: digits must be > 0")
	}

	negative := d.IsNegative()
	if negative {
		d = d.Neg()
	}

	// Shift to remove implied decimal, then floor.
	shifted := d.Shift(int32(scale)).Floor() // #nosec G115 -- scale is always a small positive int within int32 range
	raw := shifted.String()
	if idx := strings.IndexByte(raw, '.'); idx >= 0 {
		raw = raw[:idx]
	}

	// Zero-pad to exactly `digits` digit characters.
	if len(raw) < digits {
		raw = strings.Repeat("0", digits-len(raw)) + raw
	} else if len(raw) > digits {
		raw = raw[len(raw)-digits:]
	}

	// Pack digits + sign nibble into bytes.
	size := PackedSize(digits)
	out := make([]byte, size)

	// Build the nibble stream: leading 0 nibble (if needed) + digits + sign.
	nibbles := make([]byte, 0, digits+1)
	// If size*2-1 == digits, no padding nibble; otherwise first nibble = 0.
	if (size*2 - 1) > digits {
		nibbles = append(nibbles, 0)
	}
	for _, ch := range raw {
		nibbles = append(nibbles, byte(ch-'0')) // #nosec G115 -- ch is always '0'-'9'; ch-'0' is always in [0,9], within byte range
	}
	if negative {
		nibbles = append(nibbles, 0xD)
	} else {
		nibbles = append(nibbles, 0xC)
	}

	for i := 0; i < size; i++ {
		out[i] = (nibbles[i*2] << 4) | nibbles[i*2+1]
	}
	return out, nil
}
