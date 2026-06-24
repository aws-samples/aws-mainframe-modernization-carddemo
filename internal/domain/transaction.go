package domain

import (
	"fmt"

	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/cobolfmt"
	"github.com/shopspring/decimal"
)

// TransactionRecord is the Go port of TRAN-RECORD (CVTRA05Y.cpy, 350 bytes).
//
// COBOL layout:
//
//	05 TRAN-ID             PIC X(16)       offset   0 len  16
//	05 TRAN-TYPE-CD        PIC X(02)       offset  16 len   2
//	05 TRAN-CAT-CD         PIC 9(04)       offset  18 len   4
//	05 TRAN-SOURCE         PIC X(10)       offset  22 len  10
//	05 TRAN-DESC           PIC X(100)      offset  32 len 100
//	05 TRAN-AMT            PIC S9(09)V99   offset 132 len  11
//	05 TRAN-MERCHANT-ID    PIC 9(09)       offset 143 len   9
//	05 TRAN-MERCHANT-NAME  PIC X(50)       offset 152 len  50
//	05 TRAN-MERCHANT-CITY  PIC X(50)       offset 202 len  50
//	05 TRAN-MERCHANT-ZIP   PIC X(10)       offset 252 len  10
//	05 TRAN-CARD-NUM       PIC X(16)       offset 262 len  16
//	05 TRAN-ORIG-TS        PIC X(26)       offset 278 len  26
//	05 TRAN-PROC-TS        PIC X(26)       offset 304 len  26
//	05 FILLER              PIC X(20)       offset 330 len  20
type TransactionRecord struct {
	TranID           string          `cobol:"TRAN-ID,0,16,alpha"`
	TranTypeCode     string          `cobol:"TRAN-TYPE-CD,16,2,alpha"`
	TranCatCode      int64           `cobol:"TRAN-CAT-CD,18,4,zoned_uint"`
	TranSource       string          `cobol:"TRAN-SOURCE,22,10,alpha"`
	TranDesc         string          `cobol:"TRAN-DESC,32,100,alpha"`
	TranAmt          decimal.Decimal `cobol:"TRAN-AMT,132,11,zoned_s2"`
	TranMerchantID   int64           `cobol:"TRAN-MERCHANT-ID,143,9,zoned_uint"`
	TranMerchantName string          `cobol:"TRAN-MERCHANT-NAME,152,50,alpha"`
	TranMerchantCity string          `cobol:"TRAN-MERCHANT-CITY,202,50,alpha"`
	TranMerchantZip  string          `cobol:"TRAN-MERCHANT-ZIP,252,10,alpha"`
	TranCardNum      string          `cobol:"TRAN-CARD-NUM,262,16,alpha"`
	TranOrigTS       string          `cobol:"TRAN-ORIG-TS,278,26,alpha"`
	TranProcTS       string          `cobol:"TRAN-PROC-TS,304,26,alpha"`
}

// TransactionRecordLen is the fixed length of a TRAN-RECORD in bytes.
const TransactionRecordLen = 350

// DecodeTransactionRecord decodes one 350-byte EBCDIC record into a TransactionRecord.
func DecodeTransactionRecord(b []byte) (*TransactionRecord, error) {
	if len(b) < TransactionRecordLen {
		return nil, fmt.Errorf("domain: transaction: record too short: got %d, want %d", len(b), TransactionRecordLen)
	}
	b = b[:TransactionRecordLen]

	var r TransactionRecord
	var err error

	alphaFields := []struct {
		dst    *string
		offset int
		length int
		label  string
	}{
		{&r.TranID, 0, 16, "TRAN-ID"},
		{&r.TranTypeCode, 16, 2, "TRAN-TYPE-CD"},
		{&r.TranSource, 22, 10, "TRAN-SOURCE"},
		{&r.TranDesc, 32, 100, "TRAN-DESC"},
		{&r.TranMerchantName, 152, 50, "TRAN-MERCHANT-NAME"},
		{&r.TranMerchantCity, 202, 50, "TRAN-MERCHANT-CITY"},
		{&r.TranMerchantZip, 252, 10, "TRAN-MERCHANT-ZIP"},
		{&r.TranCardNum, 262, 16, "TRAN-CARD-NUM"},
		{&r.TranOrigTS, 278, 26, "TRAN-ORIG-TS"},
		{&r.TranProcTS, 304, 26, "TRAN-PROC-TS"},
	}
	for _, af := range alphaFields {
		*af.dst, err = cobolfmt.DecodeEBCDICTrimmed(b[af.offset : af.offset+af.length])
		if err != nil {
			return nil, fmt.Errorf("domain: transaction: %s: %w", af.label, err)
		}
	}

	if r.TranCatCode, err = cobolfmt.DecodeZonedUint(b[18:22]); err != nil {
		return nil, fmt.Errorf("domain: transaction: TRAN-CAT-CD: %w", err)
	}
	if r.TranAmt, err = cobolfmt.DecodeZoned(b[132:143], 2, true); err != nil {
		return nil, fmt.Errorf("domain: transaction: TRAN-AMT: %w", err)
	}
	if r.TranMerchantID, err = cobolfmt.DecodeZonedUint(b[143:152]); err != nil {
		return nil, fmt.Errorf("domain: transaction: TRAN-MERCHANT-ID: %w", err)
	}

	return &r, nil
}

// Encode serialises a TransactionRecord back to its 350-byte EBCDIC representation.
func (r *TransactionRecord) Encode() ([]byte, error) {
	out := make([]byte, TransactionRecordLen)

	alphaFields := []struct {
		val    string
		offset int
		length int
		label  string
	}{
		{r.TranID, 0, 16, "TRAN-ID"},
		{r.TranTypeCode, 16, 2, "TRAN-TYPE-CD"},
		{r.TranSource, 22, 10, "TRAN-SOURCE"},
		{r.TranDesc, 32, 100, "TRAN-DESC"},
		{r.TranMerchantName, 152, 50, "TRAN-MERCHANT-NAME"},
		{r.TranMerchantCity, 202, 50, "TRAN-MERCHANT-CITY"},
		{r.TranMerchantZip, 252, 10, "TRAN-MERCHANT-ZIP"},
		{r.TranCardNum, 262, 16, "TRAN-CARD-NUM"},
		{r.TranOrigTS, 278, 26, "TRAN-ORIG-TS"},
		{r.TranProcTS, 304, 26, "TRAN-PROC-TS"},
	}
	for _, af := range alphaFields {
		fb, ferr := cobolfmt.EncodeEBCDIC(cobolfmt.PadRight(af.val, af.length), af.length)
		if ferr != nil {
			return nil, fmt.Errorf("domain: transaction: %s: %w", af.label, ferr)
		}
		copy(out[af.offset:af.offset+af.length], fb)
	}

	fcat, err := cobolfmt.EncodeZonedUint(r.TranCatCode, 4)
	if err != nil {
		return nil, fmt.Errorf("domain: transaction: TRAN-CAT-CD: %w", err)
	}
	copy(out[18:22], fcat)

	famt, err := cobolfmt.EncodeZoned(r.TranAmt, 11, 2, true)
	if err != nil {
		return nil, fmt.Errorf("domain: transaction: TRAN-AMT: %w", err)
	}
	copy(out[132:143], famt)

	fmid, err := cobolfmt.EncodeZonedUint(r.TranMerchantID, 9)
	if err != nil {
		return nil, fmt.Errorf("domain: transaction: TRAN-MERCHANT-ID: %w", err)
	}
	copy(out[143:152], fmid)

	copy(out[330:350], cobolfmt.EBCDICSpacePad(20))
	return out, nil
}

// DailyTransactionRecord is the Go port of DALYTRAN-RECORD (CVTRA06Y.cpy, 350 bytes).
// It has the same layout as TransactionRecord with DALYTRAN- prefixed field names.
type DailyTransactionRecord struct {
	DalytranID           string          `cobol:"DALYTRAN-ID,0,16,alpha"`
	DalytranTypeCode     string          `cobol:"DALYTRAN-TYPE-CD,16,2,alpha"`
	DalytranCatCode      int64           `cobol:"DALYTRAN-CAT-CD,18,4,zoned_uint"`
	DalytranSource       string          `cobol:"DALYTRAN-SOURCE,22,10,alpha"`
	DalytranDesc         string          `cobol:"DALYTRAN-DESC,32,100,alpha"`
	DalytranAmt          decimal.Decimal `cobol:"DALYTRAN-AMT,132,11,zoned_s2"`
	DalytranMerchantID   int64           `cobol:"DALYTRAN-MERCHANT-ID,143,9,zoned_uint"`
	DalytranMerchantName string          `cobol:"DALYTRAN-MERCHANT-NAME,152,50,alpha"`
	DalytranMerchantCity string          `cobol:"DALYTRAN-MERCHANT-CITY,202,50,alpha"`
	DalytranMerchantZip  string          `cobol:"DALYTRAN-MERCHANT-ZIP,252,10,alpha"`
	DalytranCardNum      string          `cobol:"DALYTRAN-CARD-NUM,262,16,alpha"`
	DalytranOrigTS       string          `cobol:"DALYTRAN-ORIG-TS,278,26,alpha"`
	DalytranProcTS       string          `cobol:"DALYTRAN-PROC-TS,304,26,alpha"`
}

// DailyTransactionRecordLen is the fixed length of a DALYTRAN-RECORD in bytes.
const DailyTransactionRecordLen = 350

// DecodeDailyTransactionRecord decodes one 350-byte EBCDIC record into a DailyTransactionRecord.
func DecodeDailyTransactionRecord(b []byte) (*DailyTransactionRecord, error) {
	if len(b) < DailyTransactionRecordLen {
		return nil, fmt.Errorf("domain: dalytran: record too short: got %d, want %d", len(b), DailyTransactionRecordLen)
	}
	b = b[:DailyTransactionRecordLen]

	var r DailyTransactionRecord
	var err error

	alphaFields := []struct {
		dst    *string
		offset int
		length int
		label  string
	}{
		{&r.DalytranID, 0, 16, "DALYTRAN-ID"},
		{&r.DalytranTypeCode, 16, 2, "DALYTRAN-TYPE-CD"},
		{&r.DalytranSource, 22, 10, "DALYTRAN-SOURCE"},
		{&r.DalytranDesc, 32, 100, "DALYTRAN-DESC"},
		{&r.DalytranMerchantName, 152, 50, "DALYTRAN-MERCHANT-NAME"},
		{&r.DalytranMerchantCity, 202, 50, "DALYTRAN-MERCHANT-CITY"},
		{&r.DalytranMerchantZip, 252, 10, "DALYTRAN-MERCHANT-ZIP"},
		{&r.DalytranCardNum, 262, 16, "DALYTRAN-CARD-NUM"},
		{&r.DalytranOrigTS, 278, 26, "DALYTRAN-ORIG-TS"},
		{&r.DalytranProcTS, 304, 26, "DALYTRAN-PROC-TS"},
	}
	for _, af := range alphaFields {
		*af.dst, err = cobolfmt.DecodeEBCDICTrimmed(b[af.offset : af.offset+af.length])
		if err != nil {
			return nil, fmt.Errorf("domain: dalytran: %s: %w", af.label, err)
		}
	}

	if r.DalytranCatCode, err = cobolfmt.DecodeZonedUint(b[18:22]); err != nil {
		return nil, fmt.Errorf("domain: dalytran: DALYTRAN-CAT-CD: %w", err)
	}
	if r.DalytranAmt, err = cobolfmt.DecodeZoned(b[132:143], 2, true); err != nil {
		return nil, fmt.Errorf("domain: dalytran: DALYTRAN-AMT: %w", err)
	}
	if r.DalytranMerchantID, err = cobolfmt.DecodeZonedUint(b[143:152]); err != nil {
		return nil, fmt.Errorf("domain: dalytran: DALYTRAN-MERCHANT-ID: %w", err)
	}

	return &r, nil
}

// TrnxRecord is the Go port of TRNX-RECORD (COSTM01.CPY, 350 bytes).
// Used for keyed VSAM access (key = TRNX-CARD-NUM + TRNX-ID).
//
// COBOL layout:
//
//	05 TRNX-KEY:
//	   10 TRNX-CARD-NUM     PIC X(16)     offset   0 len 16
//	   10 TRNX-ID           PIC X(16)     offset  16 len 16
//	05 TRNX-REST:
//	   10 TRNX-TYPE-CD      PIC X(02)     offset  32 len  2
//	   10 TRNX-CAT-CD       PIC 9(04)     offset  34 len  4
//	   10 TRNX-SOURCE       PIC X(10)     offset  38 len 10
//	   10 TRNX-DESC         PIC X(100)    offset  48 len 100
//	   10 TRNX-AMT          PIC S9(09)V99 offset 148 len 11
//	   10 TRNX-MERCHANT-ID  PIC 9(09)     offset 159 len  9
//	   10 TRNX-MERCHANT-NAME PIC X(50)    offset 168 len 50
//	   10 TRNX-MERCHANT-CITY PIC X(50)    offset 218 len 50
//	   10 TRNX-MERCHANT-ZIP  PIC X(10)    offset 268 len 10
//	   10 TRNX-ORIG-TS      PIC X(26)     offset 278 len 26
//	   10 TRNX-PROC-TS      PIC X(26)     offset 304 len 26
//	   10 FILLER            PIC X(20)     offset 330 len 20
type TrnxRecord struct {
	TrnxCardNum      string          `cobol:"TRNX-CARD-NUM,0,16,alpha"`
	TrnxID           string          `cobol:"TRNX-ID,16,16,alpha"`
	TrnxTypeCode     string          `cobol:"TRNX-TYPE-CD,32,2,alpha"`
	TrnxCatCode      int64           `cobol:"TRNX-CAT-CD,34,4,zoned_uint"`
	TrnxSource       string          `cobol:"TRNX-SOURCE,38,10,alpha"`
	TrnxDesc         string          `cobol:"TRNX-DESC,48,100,alpha"`
	TrnxAmt          decimal.Decimal `cobol:"TRNX-AMT,148,11,zoned_s2"`
	TrnxMerchantID   int64           `cobol:"TRNX-MERCHANT-ID,159,9,zoned_uint"`
	TrnxMerchantName string          `cobol:"TRNX-MERCHANT-NAME,168,50,alpha"`
	TrnxMerchantCity string          `cobol:"TRNX-MERCHANT-CITY,218,50,alpha"`
	TrnxMerchantZip  string          `cobol:"TRNX-MERCHANT-ZIP,268,10,alpha"`
	TrnxOrigTS       string          `cobol:"TRNX-ORIG-TS,278,26,alpha"`
	TrnxProcTS       string          `cobol:"TRNX-PROC-TS,304,26,alpha"`
}

// TrnxRecordLen is the fixed length of a TRNX-RECORD in bytes.
const TrnxRecordLen = 350

// DecodeTrnxRecord decodes one 350-byte EBCDIC record into a TrnxRecord.
func DecodeTrnxRecord(b []byte) (*TrnxRecord, error) {
	if len(b) < TrnxRecordLen {
		return nil, fmt.Errorf("domain: trnx: record too short: got %d, want %d", len(b), TrnxRecordLen)
	}
	b = b[:TrnxRecordLen]

	var r TrnxRecord
	var err error

	alphaFields := []struct {
		dst    *string
		offset int
		length int
		label  string
	}{
		{&r.TrnxCardNum, 0, 16, "TRNX-CARD-NUM"},
		{&r.TrnxID, 16, 16, "TRNX-ID"},
		{&r.TrnxTypeCode, 32, 2, "TRNX-TYPE-CD"},
		{&r.TrnxSource, 38, 10, "TRNX-SOURCE"},
		{&r.TrnxDesc, 48, 100, "TRNX-DESC"},
		{&r.TrnxMerchantName, 168, 50, "TRNX-MERCHANT-NAME"},
		{&r.TrnxMerchantCity, 218, 50, "TRNX-MERCHANT-CITY"},
		{&r.TrnxMerchantZip, 268, 10, "TRNX-MERCHANT-ZIP"},
		{&r.TrnxOrigTS, 278, 26, "TRNX-ORIG-TS"},
		{&r.TrnxProcTS, 304, 26, "TRNX-PROC-TS"},
	}
	for _, af := range alphaFields {
		*af.dst, err = cobolfmt.DecodeEBCDICTrimmed(b[af.offset : af.offset+af.length])
		if err != nil {
			return nil, fmt.Errorf("domain: trnx: %s: %w", af.label, err)
		}
	}

	if r.TrnxCatCode, err = cobolfmt.DecodeZonedUint(b[34:38]); err != nil {
		return nil, fmt.Errorf("domain: trnx: TRNX-CAT-CD: %w", err)
	}
	if r.TrnxAmt, err = cobolfmt.DecodeZoned(b[148:159], 2, true); err != nil {
		return nil, fmt.Errorf("domain: trnx: TRNX-AMT: %w", err)
	}
	if r.TrnxMerchantID, err = cobolfmt.DecodeZonedUint(b[159:168]); err != nil {
		return nil, fmt.Errorf("domain: trnx: TRNX-MERCHANT-ID: %w", err)
	}

	return &r, nil
}

// DecodeTransactionRecords reads all TransactionRecords from a flat fixed-length binary file.
func DecodeTransactionRecords(data []byte) ([]*TransactionRecord, error) {
	if len(data)%TransactionRecordLen != 0 {
		return nil, fmt.Errorf("domain: transaction: data length %d is not a multiple of %d", len(data), TransactionRecordLen)
	}
	count := len(data) / TransactionRecordLen
	records := make([]*TransactionRecord, 0, count)
	for i := 0; i < count; i++ {
		rec, err := DecodeTransactionRecord(data[i*TransactionRecordLen : (i+1)*TransactionRecordLen])
		if err != nil {
			return nil, fmt.Errorf("domain: transaction: record %d: %w", i, err)
		}
		records = append(records, rec)
	}
	return records, nil
}
