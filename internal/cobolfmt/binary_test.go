package cobolfmt_test

import (
	"testing"

	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/cobolfmt"
)

func TestCompSize(t *testing.T) {
	tests := []struct {
		digits int
		want   int
	}{
		{1, 2},
		{2, 2},
		{4, 2},
		{5, 4},
		{9, 4},
		{10, 8},
		{18, 8},
	}
	for _, tc := range tests {
		got := cobolfmt.CompSize(tc.digits)
		if got != tc.want {
			t.Errorf("CompSize(%d) = %d, want %d", tc.digits, got, tc.want)
		}
	}
}

func TestDecodeComp(t *testing.T) {
	tests := []struct {
		name   string
		input  []byte
		signed bool
		want   int64
	}{
		{"unsigned 0 (2 bytes)", []byte{0x00, 0x00}, false, 0},
		{"unsigned 1 (2 bytes)", []byte{0x00, 0x01}, false, 1},
		{"unsigned 65535 (2 bytes)", []byte{0xFF, 0xFF}, false, 65535},
		{"unsigned 1 (4 bytes)", []byte{0x00, 0x00, 0x00, 0x01}, false, 1},
		{"unsigned 1000000 (4 bytes)", []byte{0x00, 0x0F, 0x42, 0x40}, false, 1000000},
		{"signed -1 (2 bytes)", []byte{0xFF, 0xFF}, true, -1},
		{"signed -1 (4 bytes)", []byte{0xFF, 0xFF, 0xFF, 0xFF}, true, -1},
		{"signed 100 (4 bytes)", []byte{0x00, 0x00, 0x00, 0x64}, true, 100},
		{"unsigned 1 (8 bytes)", []byte{0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01}, false, 1},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, err := cobolfmt.DecodeComp(tc.input, tc.signed)
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if got != tc.want {
				t.Errorf("got %d, want %d", got, tc.want)
			}
		})
	}
}

func TestEncodeCompRoundTrip(t *testing.T) {
	tests := []struct {
		value  int64
		size   int
		signed bool
	}{
		{0, 2, true},
		{1, 2, true},
		{32767, 2, true},  // max positive for signed 2-byte
		{-1, 2, true},
		{65535, 2, false}, // unsigned 2-byte max
		{100000, 4, true},
		{-1, 4, true},
		{1234567890, 8, true},
		{-9876543210, 8, true},
	}

	for _, tc := range tests {
		encoded, err := cobolfmt.EncodeComp(tc.value, tc.size)
		if err != nil {
			t.Fatalf("EncodeComp(%d, %d): %v", tc.value, tc.size, err)
		}
		if len(encoded) != tc.size {
			t.Fatalf("encoded length: got %d, want %d", len(encoded), tc.size)
		}

		decoded, err := cobolfmt.DecodeComp(encoded, tc.signed)
		if err != nil {
			t.Fatalf("DecodeComp: %v", err)
		}
		if decoded != tc.value {
			t.Errorf("round-trip(value=%d, size=%d, signed=%v): got %d, want %d", tc.value, tc.size, tc.signed, decoded, tc.value)
		}
	}
}

func TestDecodeCompError(t *testing.T) {
	_, err := cobolfmt.DecodeComp([]byte{0x00, 0x00, 0x00}, false)
	if err == nil {
		t.Error("expected error for 3-byte input, got nil")
	}
}
