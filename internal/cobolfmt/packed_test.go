package cobolfmt_test

import (
	"testing"

	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/cobolfmt"
	"github.com/shopspring/decimal"
)

func TestPackedSize(t *testing.T) {
	tests := []struct {
		digits int
		want   int
	}{
		{1, 1},
		{2, 2},
		{3, 2},
		{4, 3},
		{5, 3},
		{6, 4},
		{7, 4},
		{9, 5},
		{11, 6},
	}
	for _, tc := range tests {
		got := cobolfmt.PackedSize(tc.digits)
		if got != tc.want {
			t.Errorf("PackedSize(%d) = %d, want %d", tc.digits, got, tc.want)
		}
	}
}

func TestDecodePacked(t *testing.T) {
	tests := []struct {
		name    string
		input   []byte
		scale   int
		want    string
		wantErr bool
	}{
		{
			name:  "positive 123 (3 digits, 2 bytes)",
			input: []byte{0x01, 0x2C}, // 01 2C = digits 0,1,2 sign=C(+)
			scale: 0,
			want:  "12", // leading zero is stripped, effectively digits are 1,2
			// Actually 0x01 = hi=0,lo=1; 0x2C = hi=2,lo=C. Digits: [0,1,2], sign=C -> 12
		},
		{
			name:  "negative 456",
			input: []byte{0x04, 0x5D}, // digits 0,4,5 sign=D(-) -> -45
			scale: 0,
			want:  "-45",
		},
		{
			name:  "zero positive",
			input: []byte{0x0C}, // single byte: hi=0, lo=C -> digit 0, sign C -> 0
			scale: 0,
			want:  "0",
		},
		{
			name:  "positive with scale 2",
			input: []byte{0x01, 0x23, 0x4C}, // digits 0,1,2,3,4 sign C -> 1234 with scale2 -> 12.34
			scale: 2,
			want:  "12.34",
		},
		{
			name:  "negative with scale 2",
			input: []byte{0x00, 0x05, 0x0D}, // digits 0,0,0,5,0 sign D -> -50 scale2 -> -0.50
			scale: 2,
			want:  "-0.50",
		},
		{
			name:    "invalid sign nibble",
			input:   []byte{0x01, 0x2B}, // B is not a valid sign nibble in strict mode
			scale:   0,
			wantErr: true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, err := cobolfmt.DecodePacked(tc.input, tc.scale)
			if tc.wantErr {
				if err == nil {
					t.Errorf("expected error, got nil")
				}
				return
			}
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			want, _ := decimal.NewFromString(tc.want)
			if !got.Equal(want) {
				t.Errorf("got %s, want %s", got, want)
			}
		})
	}
}

func TestEncodePackedRoundTrip(t *testing.T) {
	tests := []struct {
		name   string
		value  string
		digits int
		scale  int
	}{
		{"positive 0", "0", 3, 0},
		{"positive 123", "123", 3, 0},
		{"negative 45", "-45", 3, 0},
		{"positive 12.34", "12.34", 5, 2},
		{"negative 0.50", "-0.50", 3, 2},
		{"odd digits 9", "9", 1, 0},
		{"even digits 99", "99", 2, 0},
		{"large 9999999.99", "9999999.99", 9, 2},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			d, err := decimal.NewFromString(tc.value)
			if err != nil {
				t.Fatalf("invalid decimal: %v", err)
			}

			encoded, err := cobolfmt.EncodePacked(d, tc.digits, tc.scale)
			if err != nil {
				t.Fatalf("EncodePacked: %v", err)
			}
			wantSize := cobolfmt.PackedSize(tc.digits)
			if len(encoded) != wantSize {
				t.Fatalf("encoded length: got %d, want %d", len(encoded), wantSize)
			}

			decoded, err := cobolfmt.DecodePacked(encoded, tc.scale)
			if err != nil {
				t.Fatalf("DecodePacked: %v", err)
			}
			if !decoded.Equal(d) {
				t.Errorf("round-trip: got %s, want %s", decoded, d)
			}
		})
	}
}
