package domain

import (
	"fmt"

	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/cobolfmt"
)

// CardRecord is the Go port of CARD-RECORD (CVACT02Y.cpy, 150 bytes).
//
// COBOL layout:
//
//	05 CARD-NUM             PIC X(16)   offset   0 len 16
//	05 CARD-ACCT-ID         PIC 9(11)   offset  16 len 11
//	05 CARD-CVV-CD          PIC 9(03)   offset  27 len  3
//	05 CARD-EMBOSSED-NAME   PIC X(50)   offset  30 len 50
//	05 CARD-EXPIRAION-DATE  PIC X(10)   offset  80 len 10
//	05 CARD-ACTIVE-STATUS   PIC X(01)   offset  90 len  1
//	05 FILLER               PIC X(59)   offset  91 len 59
type CardRecord struct {
	// CardNum is the 16-digit card number (PIC X(16)).
	CardNum string `cobol:"CARD-NUM,0,16,alpha"`
	// CardAcctID is the account ID linked to the card (PIC 9(11)).
	CardAcctID int64 `cobol:"CARD-ACCT-ID,16,11,zoned_uint"`
	// CardCVVCode is the CVV security code (PIC 9(03)).
	CardCVVCode int64 `cobol:"CARD-CVV-CD,27,3,zoned_uint"`
	// CardEmbossedName is the name on the card face (PIC X(50)).
	CardEmbossedName string `cobol:"CARD-EMBOSSED-NAME,30,50,alpha"`
	// CardExpirationDate is the expiration date YYYY-MM-DD (PIC X(10)).
	CardExpirationDate string `cobol:"CARD-EXPIRAION-DATE,80,10,alpha"`
	// CardActiveStatus is 'Y' or 'N' (PIC X(01)).
	CardActiveStatus string `cobol:"CARD-ACTIVE-STATUS,90,1,alpha"`
}

// CardRecordLen is the fixed length of a CARD-RECORD in bytes.
const CardRecordLen = 150

// DecodeCardRecord decodes one 150-byte EBCDIC record into a CardRecord.
func DecodeCardRecord(b []byte) (*CardRecord, error) {
	if len(b) < CardRecordLen {
		return nil, fmt.Errorf("domain: card: record too short: got %d, want %d", len(b), CardRecordLen)
	}
	b = b[:CardRecordLen]

	var r CardRecord
	var err error

	if r.CardNum, err = cobolfmt.DecodeEBCDICTrimmed(b[0:16]); err != nil {
		return nil, fmt.Errorf("domain: card: CARD-NUM: %w", err)
	}
	if r.CardAcctID, err = cobolfmt.DecodeZonedUint(b[16:27]); err != nil {
		return nil, fmt.Errorf("domain: card: CARD-ACCT-ID: %w", err)
	}
	if r.CardCVVCode, err = cobolfmt.DecodeZonedUint(b[27:30]); err != nil {
		return nil, fmt.Errorf("domain: card: CARD-CVV-CD: %w", err)
	}
	if r.CardEmbossedName, err = cobolfmt.DecodeEBCDICTrimmed(b[30:80]); err != nil {
		return nil, fmt.Errorf("domain: card: CARD-EMBOSSED-NAME: %w", err)
	}
	if r.CardExpirationDate, err = cobolfmt.DecodeEBCDICTrimmed(b[80:90]); err != nil {
		return nil, fmt.Errorf("domain: card: CARD-EXPIRAION-DATE: %w", err)
	}
	if r.CardActiveStatus, err = cobolfmt.DecodeEBCDICTrimmed(b[90:91]); err != nil {
		return nil, fmt.Errorf("domain: card: CARD-ACTIVE-STATUS: %w", err)
	}

	return &r, nil
}

// Encode serialises a CardRecord back to its 150-byte EBCDIC representation.
func (r *CardRecord) Encode() ([]byte, error) {
	out := make([]byte, CardRecordLen)

	for _, enc := range []struct {
		val    string
		offset int
		length int
		label  string
	}{
		{r.CardNum, 0, 16, "CARD-NUM"},
		{r.CardEmbossedName, 30, 50, "CARD-EMBOSSED-NAME"},
		{r.CardExpirationDate, 80, 10, "CARD-EXPIRAION-DATE"},
		{r.CardActiveStatus, 90, 1, "CARD-ACTIVE-STATUS"},
	} {
		fb, ferr := cobolfmt.EncodeEBCDIC(cobolfmt.PadRight(enc.val, enc.length), enc.length)
		if ferr != nil {
			return nil, fmt.Errorf("domain: card: %s: %w", enc.label, ferr)
		}
		copy(out[enc.offset:enc.offset+enc.length], fb)
	}

	fid, err := cobolfmt.EncodeZonedUint(r.CardAcctID, 11)
	if err != nil {
		return nil, fmt.Errorf("domain: card: CARD-ACCT-ID: %w", err)
	}
	copy(out[16:27], fid)

	fcvv, err := cobolfmt.EncodeZonedUint(r.CardCVVCode, 3)
	if err != nil {
		return nil, fmt.Errorf("domain: card: CARD-CVV-CD: %w", err)
	}
	copy(out[27:30], fcvv)

	copy(out[91:150], cobolfmt.EBCDICSpacePad(59))
	return out, nil
}

// DecodeCardRecords reads all CardRecords from a flat fixed-length binary file.
func DecodeCardRecords(data []byte) ([]*CardRecord, error) {
	if len(data)%CardRecordLen != 0 {
		return nil, fmt.Errorf("domain: card: data length %d is not a multiple of %d", len(data), CardRecordLen)
	}
	count := len(data) / CardRecordLen
	records := make([]*CardRecord, 0, count)
	for i := 0; i < count; i++ {
		rec, err := DecodeCardRecord(data[i*CardRecordLen : (i+1)*CardRecordLen])
		if err != nil {
			return nil, fmt.Errorf("domain: card: record %d: %w", i, err)
		}
		records = append(records, rec)
	}
	return records, nil
}

// CardXrefRecord is the Go port of CARD-XREF-RECORD (CVACT03Y.cpy, 50 bytes).
//
// COBOL layout:
//
//	05 XREF-CARD-NUM  PIC X(16)  offset  0 len 16
//	05 XREF-CUST-ID   PIC 9(09)  offset 16 len  9
//	05 XREF-ACCT-ID   PIC 9(11)  offset 25 len 11
//	05 FILLER         PIC X(14)  offset 36 len 14
type CardXrefRecord struct {
	// XrefCardNum is the card number (PIC X(16)).
	XrefCardNum string `cobol:"XREF-CARD-NUM,0,16,alpha"`
	// XrefCustID is the customer ID (PIC 9(09)).
	XrefCustID int64 `cobol:"XREF-CUST-ID,16,9,zoned_uint"`
	// XrefAcctID is the account ID (PIC 9(11)).
	XrefAcctID int64 `cobol:"XREF-ACCT-ID,25,11,zoned_uint"`
}

// CardXrefRecordLen is the fixed length of a CARD-XREF-RECORD in bytes.
const CardXrefRecordLen = 50

// DecodeCardXrefRecord decodes one 50-byte EBCDIC record into a CardXrefRecord.
func DecodeCardXrefRecord(b []byte) (*CardXrefRecord, error) {
	if len(b) < CardXrefRecordLen {
		return nil, fmt.Errorf("domain: cardxref: record too short: got %d, want %d", len(b), CardXrefRecordLen)
	}
	b = b[:CardXrefRecordLen]

	var r CardXrefRecord
	var err error

	if r.XrefCardNum, err = cobolfmt.DecodeEBCDICTrimmed(b[0:16]); err != nil {
		return nil, fmt.Errorf("domain: cardxref: XREF-CARD-NUM: %w", err)
	}
	if r.XrefCustID, err = cobolfmt.DecodeZonedUint(b[16:25]); err != nil {
		return nil, fmt.Errorf("domain: cardxref: XREF-CUST-ID: %w", err)
	}
	if r.XrefAcctID, err = cobolfmt.DecodeZonedUint(b[25:36]); err != nil {
		return nil, fmt.Errorf("domain: cardxref: XREF-ACCT-ID: %w", err)
	}

	return &r, nil
}

// Encode serialises a CardXrefRecord back to its 50-byte EBCDIC representation.
func (r *CardXrefRecord) Encode() ([]byte, error) {
	out := make([]byte, CardXrefRecordLen)

	fnum, err := cobolfmt.EncodeEBCDIC(cobolfmt.PadRight(r.XrefCardNum, 16), 16)
	if err != nil {
		return nil, fmt.Errorf("domain: cardxref: XREF-CARD-NUM: %w", err)
	}
	copy(out[0:16], fnum)

	fcust, err := cobolfmt.EncodeZonedUint(r.XrefCustID, 9)
	if err != nil {
		return nil, fmt.Errorf("domain: cardxref: XREF-CUST-ID: %w", err)
	}
	copy(out[16:25], fcust)

	facct, err := cobolfmt.EncodeZonedUint(r.XrefAcctID, 11)
	if err != nil {
		return nil, fmt.Errorf("domain: cardxref: XREF-ACCT-ID: %w", err)
	}
	copy(out[25:36], facct)

	copy(out[36:50], cobolfmt.EBCDICSpacePad(14))
	return out, nil
}

// DecodeCardXrefRecords reads all CardXrefRecords from a flat fixed-length binary file.
func DecodeCardXrefRecords(data []byte) ([]*CardXrefRecord, error) {
	if len(data)%CardXrefRecordLen != 0 {
		return nil, fmt.Errorf("domain: cardxref: data length %d is not a multiple of %d", len(data), CardXrefRecordLen)
	}
	count := len(data) / CardXrefRecordLen
	records := make([]*CardXrefRecord, 0, count)
	for i := 0; i < count; i++ {
		rec, err := DecodeCardXrefRecord(data[i*CardXrefRecordLen : (i+1)*CardXrefRecordLen])
		if err != nil {
			return nil, fmt.Errorf("domain: cardxref: record %d: %w", i, err)
		}
		records = append(records, rec)
	}
	return records, nil
}
