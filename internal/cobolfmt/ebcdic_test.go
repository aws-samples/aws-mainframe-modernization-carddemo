package cobolfmt_test

import (
	"testing"

	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/cobolfmt"
)

func TestDecodeEBCDIC(t *testing.T) {
	tests := []struct {
		name  string
		input []byte
		want  string
	}{
		// EBCDIC CP037 values for ASCII printable characters
		{
			name:  "space (0x40)",
			input: []byte{0x40},
			want:  " ",
		},
		{
			name:  "A (0xC1)",
			input: []byte{0xC1},
			want:  "A",
		},
		{
			name:  "Z (0xE9)",
			input: []byte{0xE9},
			want:  "Z",
		},
		{
			name:  "a (0x81)",
			input: []byte{0x81},
			want:  "a",
		},
		{
			name:  "z (0xA9)",
			input: []byte{0xA9},
			want:  "z",
		},
		{
			name:  "digit 0 (0xF0)",
			input: []byte{0xF0},
			want:  "0",
		},
		{
			name:  "digit 9 (0xF9)",
			input: []byte{0xF9},
			want:  "9",
		},
		{
			name:  "Immanuel (from CUSTDATA.PS)",
			input: []byte{0xC9, 0x94, 0x94, 0x81, 0x95, 0xA4, 0x85, 0x93},
			want:  "Immanuel",
		},
		{
			name:  "hyphen/minus (0x60)",
			input: []byte{0x60},
			want:  "-",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, err := cobolfmt.DecodeEBCDIC(tc.input)
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if got != tc.want {
				t.Errorf("got %q, want %q", got, tc.want)
			}
		})
	}
}

func TestEncodeEBCDICRoundTrip(t *testing.T) {
	// Round-trip test: every printable ASCII character should survive EBCDIC encode → decode.
	printable := make([]byte, 0, 95)
	for c := byte(0x20); c <= 0x7E; c++ {
		printable = append(printable, c)
	}
	input := string(printable)

	encoded, err := cobolfmt.EncodeEBCDIC(input, len(input))
	if err != nil {
		t.Fatalf("EncodeEBCDIC: %v", err)
	}

	decoded, err := cobolfmt.DecodeEBCDIC(encoded)
	if err != nil {
		t.Fatalf("DecodeEBCDIC: %v", err)
	}

	if decoded != input {
		t.Errorf("round-trip failed:\ngot  %q\nwant %q", decoded, input)
	}
}

func TestEncodeEBCDICPadding(t *testing.T) {
	// EncodeEBCDIC pads shorter strings with EBCDIC spaces (0x40).
	encoded, err := cobolfmt.EncodeEBCDIC("AB", 5)
	if err != nil {
		t.Fatalf("EncodeEBCDIC: %v", err)
	}
	if len(encoded) != 5 {
		t.Fatalf("length: got %d, want 5", len(encoded))
	}
	// bytes 2-4 should be EBCDIC space (0x40)
	for i := 2; i < 5; i++ {
		if encoded[i] != 0x40 {
			t.Errorf("byte[%d] = 0x%02X, want 0x40", i, encoded[i])
		}
	}
}

func TestDecodeEBCDICTrimmed(t *testing.T) {
	// "AB   " (padded with EBCDIC spaces) should decode to "AB"
	input := []byte{0xC1, 0xC2, 0x40, 0x40, 0x40}
	got, err := cobolfmt.DecodeEBCDICTrimmed(input)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if got != "AB" {
		t.Errorf("got %q, want %q", got, "AB")
	}
}

func TestEncodeEBCDICTruncation(t *testing.T) {
	// Strings longer than n are truncated.
	encoded, err := cobolfmt.EncodeEBCDIC("ABCDE", 3)
	if err != nil {
		t.Fatalf("EncodeEBCDIC: %v", err)
	}
	if len(encoded) != 3 {
		t.Fatalf("length: got %d, want 3", len(encoded))
	}
	decoded, _ := cobolfmt.DecodeEBCDIC(encoded)
	if decoded != "ABC" {
		t.Errorf("got %q, want %q", decoded, "ABC")
	}
}
