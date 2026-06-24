package cobolfmt

import (
	"fmt"
	"time"
)

// ParseDate8 parses a COBOL YYYYMMDD date string (8 characters).
// Returns time.Time in UTC. Returns zero time and error for blank/invalid input.
func ParseDate8(s string) (time.Time, error) {
	if isBlank(s) {
		return time.Time{}, nil
	}
	t, err := time.Parse("20060102", s)
	if err != nil {
		return time.Time{}, fmt.Errorf("cobolfmt: date: invalid YYYYMMDD %q: %w", s, err)
	}
	return t, nil
}

// ParseDate10ISO parses a COBOL YYYY-MM-DD date string (10 characters).
// Returns time.Time in UTC. Returns zero time for blank input.
func ParseDate10ISO(s string) (time.Time, error) {
	if isBlank(s) {
		return time.Time{}, nil
	}
	t, err := time.Parse("2006-01-02", s)
	if err != nil {
		return time.Time{}, fmt.Errorf("cobolfmt: date: invalid YYYY-MM-DD %q: %w", s, err)
	}
	return t, nil
}

// FormatDate8 formats a time.Time as YYYYMMDD.
// Returns 8 spaces for zero time (matches COBOL blank date convention).
func FormatDate8(t time.Time) string {
	if t.IsZero() {
		return "        "
	}
	return t.UTC().Format("20060102")
}

// FormatDate10ISO formats a time.Time as YYYY-MM-DD.
// Returns 10 spaces for zero time.
func FormatDate10ISO(t time.Time) string {
	if t.IsZero() {
		return "          "
	}
	return t.UTC().Format("2006-01-02")
}

// ParseDate6 parses a YYMMDD date string (6 characters), interpreting YY ≥ 40 as
// 19YY and YY < 40 as 20YY (common COBOL windowing rule).
func ParseDate6(s string) (time.Time, error) {
	if isBlank(s) {
		return time.Time{}, nil
	}
	if len(s) != 6 {
		return time.Time{}, fmt.Errorf("cobolfmt: date: YYMMDD must be 6 chars, got %d", len(s))
	}

	var yy int
	fmt.Sscanf(s[:2], "%d", &yy)
	century := 2000
	if yy >= 40 {
		century = 1900
	}
	full := fmt.Sprintf("%d%s", century+yy, s[2:])
	t, err := time.Parse("20060102", full)
	if err != nil {
		return time.Time{}, fmt.Errorf("cobolfmt: date: invalid YYMMDD %q: %w", s, err)
	}
	return t, nil
}

// ParseTimestamp26 parses a COBOL 26-character timestamp of the form
// "YYYY-MM-DD HH:MM:SS.ffffff" or "YYYY-MM-DDTHH:MM:SS.ffffff".
// Returns zero time for blank input.
func ParseTimestamp26(s string) (time.Time, error) {
	s = TrimRight(s)
	if s == "" {
		return time.Time{}, nil
	}
	layouts := []string{
		"2006-01-02 15:04:05.000000",
		"2006-01-02T15:04:05.000000",
		"2006-01-02 15:04:05",
		"2006-01-02T15:04:05",
	}
	for _, layout := range layouts {
		if t, err := time.Parse(layout, s); err == nil {
			return t, nil
		}
	}
	return time.Time{}, fmt.Errorf("cobolfmt: date: cannot parse timestamp %q", s)
}

// FormatTimestamp26 formats a time.Time into a 26-character COBOL timestamp string
// "YYYY-MM-DD HH:MM:SS.ffffff". Returns 26 spaces for zero time.
func FormatTimestamp26(t time.Time) string {
	if t.IsZero() {
		return "                          "
	}
	return t.UTC().Format("2006-01-02 15:04:05.000000")
}

func isBlank(s string) bool {
	for _, ch := range s {
		if ch != ' ' && ch != '0' && ch != '-' && ch != '/' {
			return false
		}
	}
	return true
}
