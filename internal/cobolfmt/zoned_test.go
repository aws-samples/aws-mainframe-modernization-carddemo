package cobolfmt_test

import (
	"testing"

	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/cobolfmt"
	"github.com/shopspring/decimal"
)

func TestDecodeZoned(t *testing.T) {
	tests := []struct {
		name    string
		input   []byte
		scale   int
		signed  bool
		want    string // decimal string representation
		wantErr bool
	}{
		{
			name:   "unsigned zero",
			input:  []byte{0xF0},
			scale:  0,
			signed: false,
			want:   "0",
		},
		{
			name:   "unsigned 123",
			input:  []byte{0xF1, 0xF2, 0xF3},
			scale:  0,
			signed: false,
			want:   "123",
		},
		{
			name:   "positive signed with C zone",
			input:  []byte{0xF1, 0xF9, 0xF4, 0xC0}, // 1940 with positive sign on last nibble
			scale:  2,
			signed: true,
			want:   "19.40",
		},
		{
			name:   "negative signed with D zone",
			input:  []byte{0xF0, 0xF5, 0xD0}, // -50 with scale 2 = -0.50
			scale:  2,
			signed: true,
			want:   "-0.50",
		},
		{
			name:   "S9(10)V99 positive 194.00 from acctdata",
			input:  []byte{0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF1, 0xF9, 0xF4, 0xF0, 0xC0},
			scale:  2,
			signed: true,
			want:   "194.00",
		},
		{
			name:   "unsigned F zone treated as positive",
			input:  []byte{0xF4, 0xF2, 0xF0}, // 420 unsigned, scale 2 = 4.20
			scale:  2,
			signed: false,
			want:   "4.20",
		},
		{
			name:   "positive with F zone on last byte",
			input:  []byte{0xF1, 0xF2, 0xF3}, // unsigned field
			scale:  0,
			signed: false,
			want:   "123",
		},
		{
			name:    "invalid digit nibble",
			input:   []byte{0xFA},
			scale:   0,
			signed:  false,
			wantErr: true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, err := cobolfmt.DecodeZoned(tc.input, tc.scale, tc.signed)
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

func TestEncodeZonedRoundTrip(t *testing.T) {
	tests := []struct {
		name   string
		value  string
		n      int
		scale  int
		signed bool
	}{
		{"unsigned 0", "0", 3, 0, false},
		{"unsigned 123", "123", 3, 0, false},
		{"positive 194.00", "194.00", 12, 2, true},
		{"positive 202.00", "202.00", 12, 2, true},
		{"negative 0.50", "-0.50", 3, 2, true},
		{"negative 1234.56", "-1234.56", 6, 2, true},
		{"zero signed", "0.00", 11, 2, true},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			d, err := decimal.NewFromString(tc.value)
			if err != nil {
				t.Fatalf("invalid decimal %q: %v", tc.value, err)
			}

			encoded, err := cobolfmt.EncodeZoned(d, tc.n, tc.scale, tc.signed)
			if err != nil {
				t.Fatalf("EncodeZoned: %v", err)
			}
			if len(encoded) != tc.n {
				t.Fatalf("encoded length: got %d, want %d", len(encoded), tc.n)
			}

			decoded, err := cobolfmt.DecodeZoned(encoded, tc.scale, tc.signed)
			if err != nil {
				t.Fatalf("DecodeZoned: %v", err)
			}

			if !decoded.Equal(d) {
				t.Errorf("round-trip mismatch: got %s, want %s", decoded, d)
			}
		})
	}
}

func TestDecodeZonedUint(t *testing.T) {
	tests := []struct {
		input []byte
		want  int64
	}{
		{[]byte{0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF1}, 1},
		{[]byte{0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF9}, 9},
		{[]byte{0xF9, 0xF9, 0xF9}, 999},
	}

	for _, tc := range tests {
		got, err := cobolfmt.DecodeZonedUint(tc.input)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if got != tc.want {
			t.Errorf("got %d, want %d", got, tc.want)
		}
	}
}

func TestEncodeZonedUint(t *testing.T) {
	tests := []struct {
		val  int64
		n    int
		want []byte
	}{
		{1, 11, []byte{0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF1}},
		{999, 3, []byte{0xF9, 0xF9, 0xF9}},
		{0, 1, []byte{0xF0}},
	}

	for _, tc := range tests {
		got, err := cobolfmt.EncodeZonedUint(tc.val, tc.n)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if len(got) != len(tc.want) {
			t.Fatalf("length: got %d, want %d", len(got), len(tc.want))
		}
		for i := range got {
			if got[i] != tc.want[i] {
				t.Errorf("byte[%d]: got 0x%02X, want 0x%02X", i, got[i], tc.want[i])
			}
		}
	}
}
