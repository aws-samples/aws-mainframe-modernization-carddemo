package domain_test

import (
	"bytes"
	"os"
	"path/filepath"
	"runtime"
	"testing"

	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/domain"
)

// fixtureDir returns the path to the EBCDIC fixture directory relative to the
// repo root.  Tests are run from internal/domain/ so we walk up two levels.
func fixtureDir(t *testing.T) string {
	t.Helper()
	_, thisFile, _, _ := runtime.Caller(0)
	// thisFile = …/internal/domain/roundtrip_test.go
	repoRoot := filepath.Join(filepath.Dir(thisFile), "..", "..")
	dir := filepath.Join(repoRoot, "app", "data", "EBCDIC")
	if _, err := os.Stat(dir); os.IsNotExist(err) {
		t.Skipf("EBCDIC fixture directory not found at %s; skipping round-trip test", dir)
	}
	return dir
}

func readFixture(t *testing.T, dir, name string) []byte {
	t.Helper()
	path := filepath.Join(dir, name)
	data, err := os.ReadFile(path)
	if err != nil {
		t.Skipf("fixture %s not found: %v", name, err)
	}
	return data
}

// TestAccountRoundTrip decodes every account record from the EBCDIC fixture
// file and re-encodes it, checking for byte-identical output.
func TestAccountRoundTrip(t *testing.T) {
	dir := fixtureDir(t)
	data := readFixture(t, dir, "AWS.M2.CARDDEMO.ACCTDATA.PS")

	if len(data)%domain.AccountRecordLen != 0 {
		t.Fatalf("fixture length %d not a multiple of %d", len(data), domain.AccountRecordLen)
	}

	count := len(data) / domain.AccountRecordLen
	for i := 0; i < count; i++ {
		raw := data[i*domain.AccountRecordLen : (i+1)*domain.AccountRecordLen]

		rec, err := domain.DecodeAccountRecord(raw)
		if err != nil {
			t.Errorf("record %d: DecodeAccountRecord: %v", i, err)
			continue
		}

		encoded, err := rec.Encode()
		if err != nil {
			t.Errorf("record %d: Encode: %v", i, err)
			continue
		}

		if !bytes.Equal(raw, encoded) {
			t.Errorf("record %d: round-trip mismatch\n  orig:    %X\n  encoded: %X", i, raw[:min(32, len(raw))], encoded[:min(32, len(encoded))])
		}
	}
	t.Logf("tested %d account records byte-for-byte", count)
}

// TestCustomerRoundTrip decodes every customer record and re-encodes it.
func TestCustomerRoundTrip(t *testing.T) {
	dir := fixtureDir(t)
	data := readFixture(t, dir, "AWS.M2.CARDDEMO.CUSTDATA.PS")

	if len(data)%domain.CustomerRecordLen != 0 {
		t.Fatalf("fixture length %d not a multiple of %d", len(data), domain.CustomerRecordLen)
	}

	count := len(data) / domain.CustomerRecordLen
	for i := 0; i < count; i++ {
		raw := data[i*domain.CustomerRecordLen : (i+1)*domain.CustomerRecordLen]

		rec, err := domain.DecodeCustomerRecord(raw)
		if err != nil {
			t.Errorf("record %d: DecodeCustomerRecord: %v", i, err)
			continue
		}

		encoded, err := rec.Encode()
		if err != nil {
			t.Errorf("record %d: Encode: %v", i, err)
			continue
		}

		if !bytes.Equal(raw, encoded) {
			t.Errorf("record %d: round-trip mismatch\n  orig:    %X\n  encoded: %X", i, raw[:min(32, len(raw))], encoded[:min(32, len(encoded))])
		}
	}
	t.Logf("tested %d customer records byte-for-byte", count)
}

// TestCardRoundTrip decodes every card record and re-encodes it.
func TestCardRoundTrip(t *testing.T) {
	dir := fixtureDir(t)
	data := readFixture(t, dir, "AWS.M2.CARDDEMO.CARDDATA.PS")

	if len(data)%domain.CardRecordLen != 0 {
		t.Fatalf("fixture length %d not a multiple of %d", len(data), domain.CardRecordLen)
	}

	count := len(data) / domain.CardRecordLen
	for i := 0; i < count; i++ {
		raw := data[i*domain.CardRecordLen : (i+1)*domain.CardRecordLen]

		rec, err := domain.DecodeCardRecord(raw)
		if err != nil {
			t.Errorf("record %d: DecodeCardRecord: %v", i, err)
			continue
		}

		encoded, err := rec.Encode()
		if err != nil {
			t.Errorf("record %d: Encode: %v", i, err)
			continue
		}

		if !bytes.Equal(raw, encoded) {
			t.Errorf("record %d: round-trip mismatch\n  orig:    %X\n  encoded: %X", i, raw[:min(32, len(raw))], encoded[:min(32, len(encoded))])
		}
	}
	t.Logf("tested %d card records byte-for-byte", count)
}

// TestCardXrefRoundTrip decodes every card cross-reference record and re-encodes it.
func TestCardXrefRoundTrip(t *testing.T) {
	dir := fixtureDir(t)
	data := readFixture(t, dir, "AWS.M2.CARDDEMO.CARDXREF.PS")

	if len(data)%domain.CardXrefRecordLen != 0 {
		t.Fatalf("fixture length %d not a multiple of %d", len(data), domain.CardXrefRecordLen)
	}

	count := len(data) / domain.CardXrefRecordLen
	for i := 0; i < count; i++ {
		raw := data[i*domain.CardXrefRecordLen : (i+1)*domain.CardXrefRecordLen]

		rec, err := domain.DecodeCardXrefRecord(raw)
		if err != nil {
			t.Errorf("record %d: DecodeCardXrefRecord: %v", i, err)
			continue
		}

		encoded, err := rec.Encode()
		if err != nil {
			t.Errorf("record %d: Encode: %v", i, err)
			continue
		}

		if !bytes.Equal(raw, encoded) {
			t.Errorf("record %d: round-trip mismatch\n  orig:    %X\n  encoded: %X", i, raw[:min(32, len(raw))], encoded[:min(32, len(encoded))])
		}
	}
	t.Logf("tested %d card-xref records byte-for-byte", count)
}

// TestTranTypeRoundTrip decodes every transaction-type record and re-encodes it.
func TestTranTypeRoundTrip(t *testing.T) {
	dir := fixtureDir(t)
	data := readFixture(t, dir, "AWS.M2.CARDDEMO.TRANTYPE.PS")

	if len(data)%domain.TranTypeRecordLen != 0 {
		t.Fatalf("fixture length %d not a multiple of %d", len(data), domain.TranTypeRecordLen)
	}

	count := len(data) / domain.TranTypeRecordLen
	for i := 0; i < count; i++ {
		raw := data[i*domain.TranTypeRecordLen : (i+1)*domain.TranTypeRecordLen]

		rec, err := domain.DecodeTranTypeRecord(raw)
		if err != nil {
			t.Errorf("record %d: DecodeTranTypeRecord: %v", i, err)
			continue
		}

		encoded, err := rec.Encode()
		if err != nil {
			t.Errorf("record %d: Encode: %v", i, err)
			continue
		}

		if !bytes.Equal(raw, encoded) {
			t.Errorf("record %d: round-trip mismatch\n  orig:    %X\n  encoded: %X", i, raw, encoded)
		}
	}
	t.Logf("tested %d tran-type records byte-for-byte", count)
}

// TestTranCatRoundTrip decodes every transaction-category record and re-encodes it.
func TestTranCatRoundTrip(t *testing.T) {
	dir := fixtureDir(t)
	data := readFixture(t, dir, "AWS.M2.CARDDEMO.TRANCATG.PS")

	if len(data)%domain.TranCatRecordLen != 0 {
		t.Fatalf("fixture length %d not a multiple of %d", len(data), domain.TranCatRecordLen)
	}

	count := len(data) / domain.TranCatRecordLen
	for i := 0; i < count; i++ {
		raw := data[i*domain.TranCatRecordLen : (i+1)*domain.TranCatRecordLen]

		rec, err := domain.DecodeTranCatRecord(raw)
		if err != nil {
			t.Errorf("record %d: DecodeTranCatRecord: %v", i, err)
			continue
		}

		encoded, err := rec.Encode()
		if err != nil {
			t.Errorf("record %d: Encode: %v", i, err)
			continue
		}

		if !bytes.Equal(raw, encoded) {
			t.Errorf("record %d: round-trip mismatch\n  orig:    %X\n  encoded: %X", i, raw, encoded)
		}
	}
	t.Logf("tested %d tran-cat records byte-for-byte", count)
}

// TestDiscGroupRoundTrip decodes every disclosure-group record and re-encodes it.
func TestDiscGroupRoundTrip(t *testing.T) {
	dir := fixtureDir(t)
	data := readFixture(t, dir, "AWS.M2.CARDDEMO.DISCGRP.PS")

	if len(data)%domain.DiscGroupRecordLen != 0 {
		t.Fatalf("fixture length %d not a multiple of %d", len(data), domain.DiscGroupRecordLen)
	}

	count := len(data) / domain.DiscGroupRecordLen
	for i := 0; i < count; i++ {
		raw := data[i*domain.DiscGroupRecordLen : (i+1)*domain.DiscGroupRecordLen]

		rec, err := domain.DecodeDiscGroupRecord(raw)
		if err != nil {
			t.Errorf("record %d: DecodeDiscGroupRecord: %v", i, err)
			continue
		}

		encoded, err := rec.Encode()
		if err != nil {
			t.Errorf("record %d: Encode: %v", i, err)
			continue
		}

		if !bytes.Equal(raw, encoded) {
			t.Errorf("record %d: round-trip mismatch\n  orig:    %X\n  encoded: %X", i, raw, encoded)
		}
	}
	t.Logf("tested %d disc-group records byte-for-byte", count)
}

// TestTranCatBalRoundTrip decodes every transaction-category-balance record and re-encodes it.
func TestTranCatBalRoundTrip(t *testing.T) {
	dir := fixtureDir(t)
	data := readFixture(t, dir, "AWS.M2.CARDDEMO.TCATBALF.PS")

	if len(data)%domain.TranCatBalRecordLen != 0 {
		t.Fatalf("fixture length %d not a multiple of %d", len(data), domain.TranCatBalRecordLen)
	}

	count := len(data) / domain.TranCatBalRecordLen
	for i := 0; i < count; i++ {
		raw := data[i*domain.TranCatBalRecordLen : (i+1)*domain.TranCatBalRecordLen]

		rec, err := domain.DecodeTranCatBalRecord(raw)
		if err != nil {
			t.Errorf("record %d: DecodeTranCatBalRecord: %v", i, err)
			continue
		}

		encoded, err := rec.Encode()
		if err != nil {
			t.Errorf("record %d: Encode: %v", i, err)
			continue
		}

		if !bytes.Equal(raw, encoded) {
			t.Errorf("record %d: round-trip mismatch\n  orig:    %X\n  encoded: %X", i, raw, encoded)
		}
	}
	t.Logf("tested %d tran-cat-bal records byte-for-byte", count)
}

// TestUserSecRoundTrip decodes every user-security record and re-encodes it.
func TestUserSecRoundTrip(t *testing.T) {
	dir := fixtureDir(t)
	data := readFixture(t, dir, "AWS.M2.CARDDEMO.USRSEC.PS")

	if len(data)%domain.UserSecRecordLen != 0 {
		t.Fatalf("fixture length %d not a multiple of %d", len(data), domain.UserSecRecordLen)
	}

	count := len(data) / domain.UserSecRecordLen
	for i := 0; i < count; i++ {
		raw := data[i*domain.UserSecRecordLen : (i+1)*domain.UserSecRecordLen]

		rec, err := domain.DecodeUserSecRecord(raw)
		if err != nil {
			t.Errorf("record %d: DecodeUserSecRecord: %v", i, err)
			continue
		}

		encoded, err := rec.Encode()
		if err != nil {
			t.Errorf("record %d: Encode: %v", i, err)
			continue
		}

		if !bytes.Equal(raw, encoded) {
			t.Errorf("record %d: round-trip mismatch\n  orig:    %X\n  encoded: %X", i, raw, encoded)
		}
	}
	t.Logf("tested %d user-sec records byte-for-byte", count)
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}
