package domain

import (
	"fmt"

	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/cobolfmt"
	"github.com/shopspring/decimal"
)

// AccountRecord is the Go port of ACCOUNT-RECORD (CVACT01Y.cpy, 300 bytes).
//
// COBOL layout:
//
//	05 ACCT-ID                PIC 9(11)       offset   0 len 11
//	05 ACCT-ACTIVE-STATUS     PIC X(01)       offset  11 len  1
//	05 ACCT-CURR-BAL          PIC S9(10)V99   offset  12 len 12
//	05 ACCT-CREDIT-LIMIT      PIC S9(10)V99   offset  24 len 12
//	05 ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99   offset  36 len 12
//	05 ACCT-OPEN-DATE         PIC X(10)       offset  48 len 10
//	05 ACCT-EXPIRAION-DATE    PIC X(10)       offset  58 len 10
//	05 ACCT-REISSUE-DATE      PIC X(10)       offset  68 len 10
//	05 ACCT-CURR-CYC-CREDIT   PIC S9(10)V99   offset  78 len 12
//	05 ACCT-CURR-CYC-DEBIT    PIC S9(10)V99   offset  90 len 12
//	05 ACCT-ADDR-ZIP          PIC X(10)       offset 102 len 10
//	05 ACCT-GROUP-ID          PIC X(10)       offset 112 len 10
//	05 FILLER                 PIC X(178)      offset 122 len 178
type AccountRecord struct {
	// AcctID is the account identifier (PIC 9(11)).
	AcctID int64 `cobol:"ACCT-ID,0,11,zoned_uint"`
	// AcctActiveStatus is 'Y' or 'N' (PIC X(01)).
	AcctActiveStatus string `cobol:"ACCT-ACTIVE-STATUS,11,1,alpha"`
	// AcctCurrBal is the current balance (PIC S9(10)V99).
	AcctCurrBal decimal.Decimal `cobol:"ACCT-CURR-BAL,12,12,zoned_s2"`
	// AcctCreditLimit is the credit limit (PIC S9(10)V99).
	AcctCreditLimit decimal.Decimal `cobol:"ACCT-CREDIT-LIMIT,24,12,zoned_s2"`
	// AcctCashCreditLimit is the cash credit limit (PIC S9(10)V99).
	AcctCashCreditLimit decimal.Decimal `cobol:"ACCT-CASH-CREDIT-LIMIT,36,12,zoned_s2"`
	// AcctOpenDate is the account open date YYYY-MM-DD (PIC X(10)).
	AcctOpenDate string `cobol:"ACCT-OPEN-DATE,48,10,alpha"`
	// AcctExpirationDate is the card expiration date YYYY-MM-DD (PIC X(10)).
	AcctExpirationDate string `cobol:"ACCT-EXPIRAION-DATE,58,10,alpha"`
	// AcctReissueDate is the reissue date YYYY-MM-DD (PIC X(10)).
	AcctReissueDate string `cobol:"ACCT-REISSUE-DATE,68,10,alpha"`
	// AcctCurrCycCredit is current cycle credits (PIC S9(10)V99).
	AcctCurrCycCredit decimal.Decimal `cobol:"ACCT-CURR-CYC-CREDIT,78,12,zoned_s2"`
	// AcctCurrCycDebit is current cycle debits (PIC S9(10)V99).
	AcctCurrCycDebit decimal.Decimal `cobol:"ACCT-CURR-CYC-DEBIT,90,12,zoned_s2"`
	// AcctAddrZip is the account ZIP code (PIC X(10)).
	AcctAddrZip string `cobol:"ACCT-ADDR-ZIP,102,10,alpha"`
	// AcctGroupID is the account group identifier (PIC X(10)).
	AcctGroupID string `cobol:"ACCT-GROUP-ID,112,10,alpha"`
}

// AccountRecordLen is the fixed length of an ACCOUNT-RECORD in bytes.
const AccountRecordLen = 300

// DecodeAccountRecord decodes one 300-byte EBCDIC record into an AccountRecord.
func DecodeAccountRecord(b []byte) (*AccountRecord, error) {
	if len(b) < AccountRecordLen {
		return nil, fmt.Errorf("domain: account: record too short: got %d, want %d", len(b), AccountRecordLen)
	}
	b = b[:AccountRecordLen]

	var r AccountRecord
	var err error

	if r.AcctID, err = cobolfmt.DecodeZonedUint(b[0:11]); err != nil {
		return nil, fmt.Errorf("domain: account: ACCT-ID: %w", err)
	}
	if r.AcctActiveStatus, err = cobolfmt.DecodeEBCDICTrimmed(b[11:12]); err != nil {
		return nil, fmt.Errorf("domain: account: ACCT-ACTIVE-STATUS: %w", err)
	}
	if r.AcctCurrBal, err = cobolfmt.DecodeZoned(b[12:24], 2, true); err != nil {
		return nil, fmt.Errorf("domain: account: ACCT-CURR-BAL: %w", err)
	}
	if r.AcctCreditLimit, err = cobolfmt.DecodeZoned(b[24:36], 2, true); err != nil {
		return nil, fmt.Errorf("domain: account: ACCT-CREDIT-LIMIT: %w", err)
	}
	if r.AcctCashCreditLimit, err = cobolfmt.DecodeZoned(b[36:48], 2, true); err != nil {
		return nil, fmt.Errorf("domain: account: ACCT-CASH-CREDIT-LIMIT: %w", err)
	}
	if r.AcctOpenDate, err = cobolfmt.DecodeEBCDICTrimmed(b[48:58]); err != nil {
		return nil, fmt.Errorf("domain: account: ACCT-OPEN-DATE: %w", err)
	}
	if r.AcctExpirationDate, err = cobolfmt.DecodeEBCDICTrimmed(b[58:68]); err != nil {
		return nil, fmt.Errorf("domain: account: ACCT-EXPIRAION-DATE: %w", err)
	}
	if r.AcctReissueDate, err = cobolfmt.DecodeEBCDICTrimmed(b[68:78]); err != nil {
		return nil, fmt.Errorf("domain: account: ACCT-REISSUE-DATE: %w", err)
	}
	if r.AcctCurrCycCredit, err = cobolfmt.DecodeZoned(b[78:90], 2, true); err != nil {
		return nil, fmt.Errorf("domain: account: ACCT-CURR-CYC-CREDIT: %w", err)
	}
	if r.AcctCurrCycDebit, err = cobolfmt.DecodeZoned(b[90:102], 2, true); err != nil {
		return nil, fmt.Errorf("domain: account: ACCT-CURR-CYC-DEBIT: %w", err)
	}
	if r.AcctAddrZip, err = cobolfmt.DecodeEBCDICTrimmed(b[102:112]); err != nil {
		return nil, fmt.Errorf("domain: account: ACCT-ADDR-ZIP: %w", err)
	}
	if r.AcctGroupID, err = cobolfmt.DecodeEBCDICTrimmed(b[112:122]); err != nil {
		return nil, fmt.Errorf("domain: account: ACCT-GROUP-ID: %w", err)
	}

	return &r, nil
}

// Encode serialises an AccountRecord back to its 300-byte EBCDIC representation.
func (r *AccountRecord) Encode() ([]byte, error) {
	out := make([]byte, AccountRecordLen)

	fid, err := cobolfmt.EncodeZonedUint(r.AcctID, 11)
	if err != nil {
		return nil, fmt.Errorf("domain: account: ACCT-ID: %w", err)
	}
	copy(out[0:11], fid)

	fstatus, err := cobolfmt.EncodeEBCDIC(cobolfmt.PadRight(r.AcctActiveStatus, 1), 1)
	if err != nil {
		return nil, fmt.Errorf("domain: account: ACCT-ACTIVE-STATUS: %w", err)
	}
	copy(out[11:12], fstatus)

	for _, enc := range []struct {
		val    decimal.Decimal
		offset int
		label  string
	}{
		{r.AcctCurrBal, 12, "ACCT-CURR-BAL"},
		{r.AcctCreditLimit, 24, "ACCT-CREDIT-LIMIT"},
		{r.AcctCashCreditLimit, 36, "ACCT-CASH-CREDIT-LIMIT"},
		{r.AcctCurrCycCredit, 78, "ACCT-CURR-CYC-CREDIT"},
		{r.AcctCurrCycDebit, 90, "ACCT-CURR-CYC-DEBIT"},
	} {
		fb, ferr := cobolfmt.EncodeZoned(enc.val, 12, 2, true)
		if ferr != nil {
			return nil, fmt.Errorf("domain: account: %s: %w", enc.label, ferr)
		}
		copy(out[enc.offset:enc.offset+12], fb)
	}

	for _, enc := range []struct {
		val    string
		offset int
		length int
		label  string
	}{
		{r.AcctOpenDate, 48, 10, "ACCT-OPEN-DATE"},
		{r.AcctExpirationDate, 58, 10, "ACCT-EXPIRAION-DATE"},
		{r.AcctReissueDate, 68, 10, "ACCT-REISSUE-DATE"},
		{r.AcctAddrZip, 102, 10, "ACCT-ADDR-ZIP"},
		{r.AcctGroupID, 112, 10, "ACCT-GROUP-ID"},
	} {
		fb, ferr := cobolfmt.EncodeEBCDIC(cobolfmt.PadRight(enc.val, enc.length), enc.length)
		if ferr != nil {
			return nil, fmt.Errorf("domain: account: %s: %w", enc.label, ferr)
		}
		copy(out[enc.offset:enc.offset+enc.length], fb)
	}

	// FILLER (bytes 122-299) stays as zero / EBCDIC spaces.
	filler := cobolfmt.EBCDICSpacePad(178)
	copy(out[122:300], filler)

	return out, nil
}

// DecodeAccountRecords reads all AccountRecords from a flat fixed-length binary file.
func DecodeAccountRecords(data []byte) ([]*AccountRecord, error) {
	if len(data)%AccountRecordLen != 0 {
		return nil, fmt.Errorf("domain: account: data length %d is not a multiple of %d", len(data), AccountRecordLen)
	}
	count := len(data) / AccountRecordLen
	records := make([]*AccountRecord, 0, count)
	for i := 0; i < count; i++ {
		rec, err := DecodeAccountRecord(data[i*AccountRecordLen : (i+1)*AccountRecordLen])
		if err != nil {
			return nil, fmt.Errorf("domain: account: record %d: %w", i, err)
		}
		records = append(records, rec)
	}
	return records, nil
}
