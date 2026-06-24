package cobolfmt

import (
	"encoding/binary"
	"fmt"
	"math"
)

// CompSize returns the storage size in bytes for a COMP (binary) field with n digits.
//
// IBM COBOL binary storage:
//   - PIC 9(1)–9(4):   2 bytes (halfword)
//   - PIC 9(5)–9(9):   4 bytes (fullword)
//   - PIC 9(10)–9(18): 8 bytes (doubleword)
func CompSize(digits int) int {
	switch {
	case digits <= 4:
		return 2
	case digits <= 9:
		return 4
	default:
		return 8
	}
}

// DecodeComp decodes a big-endian COMP (binary) integer field.
//
// Parameters:
//   - b: raw bytes (must be 2, 4, or 8 bytes).
//   - signed: true for PIC S9…, false for PIC 9….
func DecodeComp(b []byte, signed bool) (int64, error) {
	switch len(b) {
	case 2:
		if signed {
			return int64(int16(binary.BigEndian.Uint16(b))), nil // #nosec G115 -- intentional 2's-complement reinterpretation of COBOL COMP signed halfword
		}
		return int64(binary.BigEndian.Uint16(b)), nil
	case 4:
		if signed {
			return int64(int32(binary.BigEndian.Uint32(b))), nil // #nosec G115 -- intentional 2's-complement reinterpretation of COBOL COMP signed fullword
		}
		return int64(binary.BigEndian.Uint32(b)), nil
	case 8:
		if signed {
			return int64(binary.BigEndian.Uint64(b)), nil // #nosec G115 -- intentional 2's-complement reinterpretation of COBOL COMP signed doubleword
		}
		u := binary.BigEndian.Uint64(b)
		if u > (1<<63 - 1) {
			return 0, fmt.Errorf("cobolfmt: binary: unsigned value %d overflows int64", u)
		}
		return int64(u), nil
	default:
		return 0, fmt.Errorf("cobolfmt: binary: unsupported field size %d (must be 2, 4, or 8)", len(b))
	}
}

// EncodeComp encodes an integer value into a big-endian COMP field.
//
// Parameters:
//   - v: value to encode.
//   - size: field size in bytes (2, 4, or 8).
func EncodeComp(v int64, size int) ([]byte, error) {
	out := make([]byte, size)
	switch size {
	case 2:
		// Accepts both signed range (-32768..32767) and unsigned range (0..65535).
		if v > math.MaxUint16 || v < math.MinInt16 {
			return nil, fmt.Errorf("cobolfmt: binary: value %d overflows 2-byte COMP field (range %d..%d)", v, math.MinInt16, math.MaxUint16)
		}
		binary.BigEndian.PutUint16(out, uint16(v)) // #nosec G115 -- range-checked above
	case 4:
		// Accepts both signed range (-2147483648..2147483647) and unsigned range (0..4294967295).
		if v > math.MaxUint32 || v < math.MinInt32 {
			return nil, fmt.Errorf("cobolfmt: binary: value %d overflows 4-byte COMP field (range %d..%d)", v, math.MinInt32, math.MaxUint32)
		}
		binary.BigEndian.PutUint32(out, uint32(v)) // #nosec G115 -- range-checked above
	case 8:
		binary.BigEndian.PutUint64(out, uint64(v)) // #nosec G115 -- int64->uint64 bit-preserving reinterpretation for COMP doubleword
	default:
		return nil, fmt.Errorf("cobolfmt: binary: unsupported field size %d (must be 2, 4, or 8)", size)
	}
	return out, nil
}

// DecodeCompDigits is a convenience function that selects the field size based on
// the number of PIC digits and decodes the value.
func DecodeCompDigits(b []byte, digits int, signed bool) (int64, error) {
	expected := CompSize(digits)
	if len(b) != expected {
		return 0, fmt.Errorf("cobolfmt: binary: expected %d bytes for %d-digit COMP field, got %d", expected, digits, len(b))
	}
	return DecodeComp(b, signed)
}
