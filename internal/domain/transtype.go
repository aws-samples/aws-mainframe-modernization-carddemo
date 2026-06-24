package domain

import (
	"fmt"

	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/cobolfmt"
	"github.com/shopspring/decimal"
)

// TranTypeRecord is the Go port of TRAN-TYPE-RECORD (CVTRA03Y.cpy, 60 bytes).
//
// COBOL layout:
//
//	05 TRAN-TYPE       PIC X(02)  offset  0 len  2
//	05 TRAN-TYPE-DESC  PIC X(50)  offset  2 len 50
//	05 FILLER          PIC X(08)  offset 52 len  8
type TranTypeRecord struct {
	TranType     string `cobol:"TRAN-TYPE,0,2,alpha"`
	TranTypeDesc string `cobol:"TRAN-TYPE-DESC,2,50,alpha"`
}

// TranTypeRecordLen is the fixed length of a TRAN-TYPE-RECORD in bytes.
const TranTypeRecordLen = 60

// DecodeTranTypeRecord decodes one 60-byte EBCDIC record into a TranTypeRecord.
func DecodeTranTypeRecord(b []byte) (*TranTypeRecord, error) {
	if len(b) < TranTypeRecordLen {
		return nil, fmt.Errorf("domain: trantype: record too short: got %d, want %d", len(b), TranTypeRecordLen)
	}
	b = b[:TranTypeRecordLen]

	var r TranTypeRecord
	var err error

	if r.TranType, err = cobolfmt.DecodeEBCDICTrimmed(b[0:2]); err != nil {
		return nil, fmt.Errorf("domain: trantype: TRAN-TYPE: %w", err)
	}
	if r.TranTypeDesc, err = cobolfmt.DecodeEBCDICTrimmed(b[2:52]); err != nil {
		return nil, fmt.Errorf("domain: trantype: TRAN-TYPE-DESC: %w", err)
	}

	return &r, nil
}

// Encode serialises a TranTypeRecord back to its 60-byte EBCDIC representation.
func (r *TranTypeRecord) Encode() ([]byte, error) {
	out := make([]byte, TranTypeRecordLen)

	ft, err := cobolfmt.EncodeEBCDIC(cobolfmt.PadRight(r.TranType, 2), 2)
	if err != nil {
		return nil, fmt.Errorf("domain: trantype: TRAN-TYPE: %w", err)
	}
	copy(out[0:2], ft)

	fd, err := cobolfmt.EncodeEBCDIC(cobolfmt.PadRight(r.TranTypeDesc, 50), 50)
	if err != nil {
		return nil, fmt.Errorf("domain: trantype: TRAN-TYPE-DESC: %w", err)
	}
	copy(out[2:52], fd)

	// FILLER is initialized to EBCDIC zeros (numeric initialization) in the fixtures.
	copy(out[52:60], cobolfmt.EBCDICZeroPad(8))
	return out, nil
}

// DecodeTranTypeRecords reads all TranTypeRecords from a flat fixed-length binary file.
func DecodeTranTypeRecords(data []byte) ([]*TranTypeRecord, error) {
	if len(data)%TranTypeRecordLen != 0 {
		return nil, fmt.Errorf("domain: trantype: data length %d is not a multiple of %d", len(data), TranTypeRecordLen)
	}
	count := len(data) / TranTypeRecordLen
	records := make([]*TranTypeRecord, 0, count)
	for i := 0; i < count; i++ {
		rec, err := DecodeTranTypeRecord(data[i*TranTypeRecordLen : (i+1)*TranTypeRecordLen])
		if err != nil {
			return nil, fmt.Errorf("domain: trantype: record %d: %w", i, err)
		}
		records = append(records, rec)
	}
	return records, nil
}

// TranCatRecord is the Go port of TRAN-CAT-RECORD (CVTRA04Y.cpy, 60 bytes).
//
// COBOL layout:
//
//	05 TRAN-CAT-KEY:
//	   10 TRAN-TYPE-CD       PIC X(02)  offset  0 len  2
//	   10 TRAN-CAT-CD        PIC 9(04)  offset  2 len  4
//	05 TRAN-CAT-TYPE-DESC    PIC X(50)  offset  6 len 50
//	05 FILLER                PIC X(04)  offset 56 len  4
type TranCatRecord struct {
	TranTypeCode    string `cobol:"TRAN-TYPE-CD,0,2,alpha"`
	TranCatCode     int64  `cobol:"TRAN-CAT-CD,2,4,zoned_uint"`
	TranCatTypeDesc string `cobol:"TRAN-CAT-TYPE-DESC,6,50,alpha"`
}

// TranCatRecordLen is the fixed length of a TRAN-CAT-RECORD in bytes.
const TranCatRecordLen = 60

// DecodeTranCatRecord decodes one 60-byte EBCDIC record into a TranCatRecord.
func DecodeTranCatRecord(b []byte) (*TranCatRecord, error) {
	if len(b) < TranCatRecordLen {
		return nil, fmt.Errorf("domain: trancat: record too short: got %d, want %d", len(b), TranCatRecordLen)
	}
	b = b[:TranCatRecordLen]

	var r TranCatRecord
	var err error

	if r.TranTypeCode, err = cobolfmt.DecodeEBCDICTrimmed(b[0:2]); err != nil {
		return nil, fmt.Errorf("domain: trancat: TRAN-TYPE-CD: %w", err)
	}
	if r.TranCatCode, err = cobolfmt.DecodeZonedUint(b[2:6]); err != nil {
		return nil, fmt.Errorf("domain: trancat: TRAN-CAT-CD: %w", err)
	}
	if r.TranCatTypeDesc, err = cobolfmt.DecodeEBCDICTrimmed(b[6:56]); err != nil {
		return nil, fmt.Errorf("domain: trancat: TRAN-CAT-TYPE-DESC: %w", err)
	}

	return &r, nil
}

// Encode serialises a TranCatRecord back to its 60-byte EBCDIC representation.
func (r *TranCatRecord) Encode() ([]byte, error) {
	out := make([]byte, TranCatRecordLen)

	ft, err := cobolfmt.EncodeEBCDIC(cobolfmt.PadRight(r.TranTypeCode, 2), 2)
	if err != nil {
		return nil, fmt.Errorf("domain: trancat: TRAN-TYPE-CD: %w", err)
	}
	copy(out[0:2], ft)

	fc, err := cobolfmt.EncodeZonedUint(r.TranCatCode, 4)
	if err != nil {
		return nil, fmt.Errorf("domain: trancat: TRAN-CAT-CD: %w", err)
	}
	copy(out[2:6], fc)

	fd, err := cobolfmt.EncodeEBCDIC(cobolfmt.PadRight(r.TranCatTypeDesc, 50), 50)
	if err != nil {
		return nil, fmt.Errorf("domain: trancat: TRAN-CAT-TYPE-DESC: %w", err)
	}
	copy(out[6:56], fd)

	// FILLER is initialized to EBCDIC zeros in the fixtures.
	copy(out[56:60], cobolfmt.EBCDICZeroPad(4))
	return out, nil
}

// DecodeTranCatRecords reads all TranCatRecords from a flat fixed-length binary file.
func DecodeTranCatRecords(data []byte) ([]*TranCatRecord, error) {
	if len(data)%TranCatRecordLen != 0 {
		return nil, fmt.Errorf("domain: trancat: data length %d is not a multiple of %d", len(data), TranCatRecordLen)
	}
	count := len(data) / TranCatRecordLen
	records := make([]*TranCatRecord, 0, count)
	for i := 0; i < count; i++ {
		rec, err := DecodeTranCatRecord(data[i*TranCatRecordLen : (i+1)*TranCatRecordLen])
		if err != nil {
			return nil, fmt.Errorf("domain: trancat: record %d: %w", i, err)
		}
		records = append(records, rec)
	}
	return records, nil
}

// TranCatBalRecord is the Go port of TRAN-CAT-BAL-RECORD (CVTRA01Y.cpy, 50 bytes).
//
// COBOL layout:
//
//	05 TRAN-CAT-KEY:
//	   10 TRANCAT-ACCT-ID   PIC 9(11)      offset  0 len 11
//	   10 TRANCAT-TYPE-CD   PIC X(02)      offset 11 len  2
//	   10 TRANCAT-CD        PIC 9(04)      offset 13 len  4
//	05 TRAN-CAT-BAL         PIC S9(09)V99  offset 17 len 11
//	05 FILLER               PIC X(22)      offset 28 len 22
type TranCatBalRecord struct {
	TrancatAcctID  int64           `cobol:"TRANCAT-ACCT-ID,0,11,zoned_uint"`
	TrancatTypeCD  string          `cobol:"TRANCAT-TYPE-CD,11,2,alpha"`
	TrancatCode    int64           `cobol:"TRANCAT-CD,13,4,zoned_uint"`
	TranCatBalance decimal.Decimal `cobol:"TRAN-CAT-BAL,17,11,zoned_s2"`
}

// TranCatBalRecordLen is the fixed length of a TRAN-CAT-BAL-RECORD in bytes.
const TranCatBalRecordLen = 50

// DecodeTranCatBalRecord decodes one 50-byte EBCDIC record into a TranCatBalRecord.
func DecodeTranCatBalRecord(b []byte) (*TranCatBalRecord, error) {
	if len(b) < TranCatBalRecordLen {
		return nil, fmt.Errorf("domain: tcatbal: record too short: got %d, want %d", len(b), TranCatBalRecordLen)
	}
	b = b[:TranCatBalRecordLen]

	var r TranCatBalRecord
	var err error

	if r.TrancatAcctID, err = cobolfmt.DecodeZonedUint(b[0:11]); err != nil {
		return nil, fmt.Errorf("domain: tcatbal: TRANCAT-ACCT-ID: %w", err)
	}
	if r.TrancatTypeCD, err = cobolfmt.DecodeEBCDICTrimmed(b[11:13]); err != nil {
		return nil, fmt.Errorf("domain: tcatbal: TRANCAT-TYPE-CD: %w", err)
	}
	if r.TrancatCode, err = cobolfmt.DecodeZonedUint(b[13:17]); err != nil {
		return nil, fmt.Errorf("domain: tcatbal: TRANCAT-CD: %w", err)
	}
	if r.TranCatBalance, err = cobolfmt.DecodeZoned(b[17:28], 2, true); err != nil {
		return nil, fmt.Errorf("domain: tcatbal: TRAN-CAT-BAL: %w", err)
	}

	return &r, nil
}

// Encode serialises a TranCatBalRecord back to its 50-byte EBCDIC representation.
func (r *TranCatBalRecord) Encode() ([]byte, error) {
	out := make([]byte, TranCatBalRecordLen)

	fid, err := cobolfmt.EncodeZonedUint(r.TrancatAcctID, 11)
	if err != nil {
		return nil, fmt.Errorf("domain: tcatbal: TRANCAT-ACCT-ID: %w", err)
	}
	copy(out[0:11], fid)

	ft, err := cobolfmt.EncodeEBCDIC(cobolfmt.PadRight(r.TrancatTypeCD, 2), 2)
	if err != nil {
		return nil, fmt.Errorf("domain: tcatbal: TRANCAT-TYPE-CD: %w", err)
	}
	copy(out[11:13], ft)

	fc, err := cobolfmt.EncodeZonedUint(r.TrancatCode, 4)
	if err != nil {
		return nil, fmt.Errorf("domain: tcatbal: TRANCAT-CD: %w", err)
	}
	copy(out[13:17], fc)

	fb, err := cobolfmt.EncodeZoned(r.TranCatBalance, 11, 2, true)
	if err != nil {
		return nil, fmt.Errorf("domain: tcatbal: TRAN-CAT-BAL: %w", err)
	}
	copy(out[17:28], fb)

	// FILLER is initialized to EBCDIC zeros in the fixtures.
	copy(out[28:50], cobolfmt.EBCDICZeroPad(22))
	return out, nil
}

// DecodeTranCatBalRecords reads all TranCatBalRecords from a flat fixed-length binary file.
func DecodeTranCatBalRecords(data []byte) ([]*TranCatBalRecord, error) {
	if len(data)%TranCatBalRecordLen != 0 {
		return nil, fmt.Errorf("domain: tcatbal: data length %d is not a multiple of %d", len(data), TranCatBalRecordLen)
	}
	count := len(data) / TranCatBalRecordLen
	records := make([]*TranCatBalRecord, 0, count)
	for i := 0; i < count; i++ {
		rec, err := DecodeTranCatBalRecord(data[i*TranCatBalRecordLen : (i+1)*TranCatBalRecordLen])
		if err != nil {
			return nil, fmt.Errorf("domain: tcatbal: record %d: %w", i, err)
		}
		records = append(records, rec)
	}
	return records, nil
}

// DiscGroupRecord is the Go port of DIS-GROUP-RECORD (CVTRA02Y.cpy, 50 bytes).
//
// COBOL layout:
//
//	05 DIS-GROUP-KEY:
//	   10 DIS-ACCT-GROUP-ID  PIC X(10)      offset  0 len 10
//	   10 DIS-TRAN-TYPE-CD   PIC X(02)      offset 10 len  2
//	   10 DIS-TRAN-CAT-CD    PIC 9(04)      offset 12 len  4
//	05 DIS-INT-RATE          PIC S9(04)V99  offset 16 len  6
//	05 FILLER                PIC X(28)      offset 22 len 28
type DiscGroupRecord struct {
	DisAcctGroupID string          `cobol:"DIS-ACCT-GROUP-ID,0,10,alpha"`
	DisTranTypeCD  string          `cobol:"DIS-TRAN-TYPE-CD,10,2,alpha"`
	DisTranCatCode int64           `cobol:"DIS-TRAN-CAT-CD,12,4,zoned_uint"`
	DisIntRate     decimal.Decimal `cobol:"DIS-INT-RATE,16,6,zoned_s2"`
}

// DiscGroupRecordLen is the fixed length of a DIS-GROUP-RECORD in bytes.
const DiscGroupRecordLen = 50

// DecodeDiscGroupRecord decodes one 50-byte EBCDIC record into a DiscGroupRecord.
func DecodeDiscGroupRecord(b []byte) (*DiscGroupRecord, error) {
	if len(b) < DiscGroupRecordLen {
		return nil, fmt.Errorf("domain: discgrp: record too short: got %d, want %d", len(b), DiscGroupRecordLen)
	}
	b = b[:DiscGroupRecordLen]

	var r DiscGroupRecord
	var err error

	if r.DisAcctGroupID, err = cobolfmt.DecodeEBCDICTrimmed(b[0:10]); err != nil {
		return nil, fmt.Errorf("domain: discgrp: DIS-ACCT-GROUP-ID: %w", err)
	}
	if r.DisTranTypeCD, err = cobolfmt.DecodeEBCDICTrimmed(b[10:12]); err != nil {
		return nil, fmt.Errorf("domain: discgrp: DIS-TRAN-TYPE-CD: %w", err)
	}
	if r.DisTranCatCode, err = cobolfmt.DecodeZonedUint(b[12:16]); err != nil {
		return nil, fmt.Errorf("domain: discgrp: DIS-TRAN-CAT-CD: %w", err)
	}
	if r.DisIntRate, err = cobolfmt.DecodeZoned(b[16:22], 2, true); err != nil {
		return nil, fmt.Errorf("domain: discgrp: DIS-INT-RATE: %w", err)
	}

	return &r, nil
}

// Encode serialises a DiscGroupRecord back to its 50-byte EBCDIC representation.
func (r *DiscGroupRecord) Encode() ([]byte, error) {
	out := make([]byte, DiscGroupRecordLen)

	fg, err := cobolfmt.EncodeEBCDIC(cobolfmt.PadRight(r.DisAcctGroupID, 10), 10)
	if err != nil {
		return nil, fmt.Errorf("domain: discgrp: DIS-ACCT-GROUP-ID: %w", err)
	}
	copy(out[0:10], fg)

	ft, err := cobolfmt.EncodeEBCDIC(cobolfmt.PadRight(r.DisTranTypeCD, 2), 2)
	if err != nil {
		return nil, fmt.Errorf("domain: discgrp: DIS-TRAN-TYPE-CD: %w", err)
	}
	copy(out[10:12], ft)

	fc, err := cobolfmt.EncodeZonedUint(r.DisTranCatCode, 4)
	if err != nil {
		return nil, fmt.Errorf("domain: discgrp: DIS-TRAN-CAT-CD: %w", err)
	}
	copy(out[12:16], fc)

	fr, err := cobolfmt.EncodeZoned(r.DisIntRate, 6, 2, true)
	if err != nil {
		return nil, fmt.Errorf("domain: discgrp: DIS-INT-RATE: %w", err)
	}
	copy(out[16:22], fr)

	// FILLER is initialized to EBCDIC zeros in the fixtures.
	copy(out[22:50], cobolfmt.EBCDICZeroPad(28))
	return out, nil
}

// DecodeDiscGroupRecords reads all DiscGroupRecords from a flat fixed-length binary file.
func DecodeDiscGroupRecords(data []byte) ([]*DiscGroupRecord, error) {
	if len(data)%DiscGroupRecordLen != 0 {
		return nil, fmt.Errorf("domain: discgrp: data length %d is not a multiple of %d", len(data), DiscGroupRecordLen)
	}
	count := len(data) / DiscGroupRecordLen
	records := make([]*DiscGroupRecord, 0, count)
	for i := 0; i < count; i++ {
		rec, err := DecodeDiscGroupRecord(data[i*DiscGroupRecordLen : (i+1)*DiscGroupRecordLen])
		if err != nil {
			return nil, fmt.Errorf("domain: discgrp: record %d: %w", i, err)
		}
		records = append(records, rec)
	}
	return records, nil
}
