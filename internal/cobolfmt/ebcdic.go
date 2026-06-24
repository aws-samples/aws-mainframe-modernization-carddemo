// Package cobolfmt provides helpers for COBOL data-format conversions.
package cobolfmt

import (
	"bytes"
	"strings"

	"golang.org/x/text/encoding/charmap"
)

// ebcdicCP037 is the EBCDIC code page 037 codec used by IBM z/OS (North America).
var ebcdicCP037 = charmap.CodePage037

// ebcdicSpace is the EBCDIC representation of a space character (0x40).
const ebcdicSpace = byte(0x40)

// DecodeEBCDIC converts a []byte in EBCDIC CP037 to a UTF-8 string.
func DecodeEBCDIC(b []byte) (string, error) {
	dec := ebcdicCP037.NewDecoder()
	out, err := dec.Bytes(b)
	if err != nil {
		return "", err
	}
	return string(out), nil
}

// DecodeEBCDICTrimmed converts EBCDIC CP037 bytes to UTF-8 and trims trailing spaces.
func DecodeEBCDICTrimmed(b []byte) (string, error) {
	s, err := DecodeEBCDIC(b)
	if err != nil {
		return "", err
	}
	return strings.TrimRight(s, " "), nil
}

// EncodeEBCDIC converts a UTF-8 string to a fixed-width EBCDIC CP037 []byte of
// exactly n bytes. Shorter strings are right-padded with EBCDIC spaces (0x40).
// Longer strings are silently truncated to n bytes.
func EncodeEBCDIC(s string, n int) ([]byte, error) {
	enc := ebcdicCP037.NewEncoder()
	encoded, err := enc.Bytes([]byte(s))
	if err != nil {
		return nil, err
	}
	result := make([]byte, n)
	// Fill with EBCDIC spaces first.
	for i := range result {
		result[i] = ebcdicSpace
	}
	copy(result, encoded)
	return result, nil
}

// EncodeEBCDICRaw encodes a UTF-8 string and returns the raw EBCDIC bytes without
// any padding or truncation. Useful when the caller controls the exact field length.
func EncodeEBCDICRaw(s string) ([]byte, error) {
	enc := ebcdicCP037.NewEncoder()
	return enc.Bytes([]byte(s))
}

// PadRight pads or truncates a string to exactly n bytes in the Go string sense,
// space-padding on the right. This mirrors COBOL MOVE of a shorter string into a
// PIC X(n) field. Use before EncodeEBCDIC when you want to control ASCII padding.
func PadRight(s string, n int) string {
	if len(s) >= n {
		return s[:n]
	}
	return s + strings.Repeat(" ", n-len(s))
}

// TrimRight removes trailing ASCII spaces from a field read back from ASCII storage.
func TrimRight(s string) string {
	return strings.TrimRight(s, " ")
}

// EBCDICSpacePad returns n EBCDIC space bytes (0x40). Useful for blank fillers.
func EBCDICSpacePad(n int) []byte {
	return bytes.Repeat([]byte{ebcdicSpace}, n)
}

// EBCDICZeroPad returns n EBCDIC '0' bytes (0xF0). Used for numeric FILLER fields
// that are initialized to ZEROS rather than SPACES (e.g. TranType, DiscGroup records).
func EBCDICZeroPad(n int) []byte {
	return bytes.Repeat([]byte{0xF0}, n)
}
