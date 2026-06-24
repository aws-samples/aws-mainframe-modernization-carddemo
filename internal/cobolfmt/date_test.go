package cobolfmt_test

import (
	"testing"
	"time"

	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/cobolfmt"
)

func TestParseDate10ISO(t *testing.T) {
	tests := []struct {
		input   string
		wantY   int
		wantM   time.Month
		wantD   int
		wantErr bool
	}{
		{"2014-11-20", 2014, time.November, 20, false},
		{"2025-05-20", 2025, time.May, 20, false},
		{"1961-06-08", 1961, time.June, 8, false},
		{"          ", 0, 0, 0, false},  // blank → zero time
		{"0000-00-00", 0, 0, 0, false},  // all zeros → zero time
		{"bad-date", 0, 0, 0, true},
	}

	for _, tc := range tests {
		t.Run(tc.input, func(t *testing.T) {
			got, err := cobolfmt.ParseDate10ISO(tc.input)
			if tc.wantErr {
				if err == nil {
					t.Errorf("expected error, got nil")
				}
				return
			}
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if tc.wantY == 0 {
				if !got.IsZero() {
					t.Errorf("expected zero time, got %v", got)
				}
				return
			}
			if got.Year() != tc.wantY || got.Month() != tc.wantM || got.Day() != tc.wantD {
				t.Errorf("got %v, want %d-%02d-%02d", got, tc.wantY, tc.wantM, tc.wantD)
			}
		})
	}
}

func TestParseDate8(t *testing.T) {
	tests := []struct {
		input string
		wantY int
		wantM time.Month
		wantD int
	}{
		{"20141120", 2014, time.November, 20},
		{"19610608", 1961, time.June, 8},
	}

	for _, tc := range tests {
		got, err := cobolfmt.ParseDate8(tc.input)
		if err != nil {
			t.Fatalf("ParseDate8(%q): %v", tc.input, err)
		}
		if got.Year() != tc.wantY || got.Month() != tc.wantM || got.Day() != tc.wantD {
			t.Errorf("got %v, want %d-%02d-%02d", got, tc.wantY, tc.wantM, tc.wantD)
		}
	}
}

func TestFormatDate10ISO(t *testing.T) {
	d := time.Date(2014, time.November, 20, 0, 0, 0, 0, time.UTC)
	got := cobolfmt.FormatDate10ISO(d)
	if got != "2014-11-20" {
		t.Errorf("got %q, want %q", got, "2014-11-20")
	}
}

func TestFormatDate10ISOZero(t *testing.T) {
	got := cobolfmt.FormatDate10ISO(time.Time{})
	if len(got) != 10 {
		t.Errorf("zero time format: got %q (len %d), want 10 spaces", got, len(got))
	}
}

func TestParseDate6LeapYear(t *testing.T) {
	// 2000-02-29 is valid (leap year)
	got, err := cobolfmt.ParseDate6("000229")
	if err != nil {
		t.Fatalf("ParseDate6(000229): %v", err)
	}
	if got.Year() != 2000 || got.Month() != time.February || got.Day() != 29 {
		t.Errorf("got %v, want 2000-02-29", got)
	}
}

func TestParseDate6Windowing(t *testing.T) {
	// YY=40 → 1940, YY=39 → 2039
	tests := []struct {
		input string
		wantY int
	}{
		{"400101", 1940},
		{"390101", 2039},
		{"000101", 2000},
		{"990101", 1999},
	}
	for _, tc := range tests {
		got, err := cobolfmt.ParseDate6(tc.input)
		if err != nil {
			t.Fatalf("ParseDate6(%q): %v", tc.input, err)
		}
		if got.Year() != tc.wantY {
			t.Errorf("input %q: got year %d, want %d", tc.input, got.Year(), tc.wantY)
		}
	}
}

func TestTimestamp26RoundTrip(t *testing.T) {
	orig := "2023-11-15 14:30:00.000000"
	ts, err := cobolfmt.ParseTimestamp26(orig)
	if err != nil {
		t.Fatalf("ParseTimestamp26: %v", err)
	}
	got := cobolfmt.FormatTimestamp26(ts)
	if got != orig {
		t.Errorf("round-trip: got %q, want %q", got, orig)
	}
}
