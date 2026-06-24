package domain

import (
	"fmt"

	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/cobolfmt"
)

// CustomerRecord is the Go port of CUSTOMER-RECORD (CVCUS01Y.cpy / CUSTREC.cpy, 500 bytes).
//
// COBOL layout (CVCUS01Y.cpy):
//
//	05 CUST-ID                PIC 9(09)    offset   0 len   9
//	05 CUST-FIRST-NAME        PIC X(25)    offset   9 len  25
//	05 CUST-MIDDLE-NAME       PIC X(25)    offset  34 len  25
//	05 CUST-LAST-NAME         PIC X(25)    offset  59 len  25
//	05 CUST-ADDR-LINE-1       PIC X(50)    offset  84 len  50
//	05 CUST-ADDR-LINE-2       PIC X(50)    offset 134 len  50
//	05 CUST-ADDR-LINE-3       PIC X(50)    offset 184 len  50
//	05 CUST-ADDR-STATE-CD     PIC X(02)    offset 234 len   2
//	05 CUST-ADDR-COUNTRY-CD   PIC X(03)    offset 236 len   3
//	05 CUST-ADDR-ZIP          PIC X(10)    offset 239 len  10
//	05 CUST-PHONE-NUM-1       PIC X(15)    offset 249 len  15
//	05 CUST-PHONE-NUM-2       PIC X(15)    offset 264 len  15
//	05 CUST-SSN               PIC 9(09)    offset 279 len   9
//	05 CUST-GOVT-ISSUED-ID    PIC X(20)    offset 288 len  20
//	05 CUST-DOB-YYYY-MM-DD    PIC X(10)    offset 308 len  10
//	05 CUST-EFT-ACCOUNT-ID    PIC X(10)    offset 318 len  10
//	05 CUST-PRI-CARD-HOLDER-IND PIC X(01)  offset 328 len   1
//	05 CUST-FICO-CREDIT-SCORE PIC 9(03)    offset 329 len   3
//	05 FILLER                 PIC X(168)   offset 332 len 168
type CustomerRecord struct {
	// CustID is the customer identifier (PIC 9(09)).
	CustID int64 `cobol:"CUST-ID,0,9,zoned_uint"`
	// CustFirstName is the customer's first name (PIC X(25)).
	CustFirstName string `cobol:"CUST-FIRST-NAME,9,25,alpha"`
	// CustMiddleName is the customer's middle name (PIC X(25)).
	CustMiddleName string `cobol:"CUST-MIDDLE-NAME,34,25,alpha"`
	// CustLastName is the customer's last name (PIC X(25)).
	CustLastName string `cobol:"CUST-LAST-NAME,59,25,alpha"`
	// CustAddrLine1 is address line 1 (PIC X(50)).
	CustAddrLine1 string `cobol:"CUST-ADDR-LINE-1,84,50,alpha"`
	// CustAddrLine2 is address line 2 (PIC X(50)).
	CustAddrLine2 string `cobol:"CUST-ADDR-LINE-2,134,50,alpha"`
	// CustAddrLine3 is address line 3 (PIC X(50)).
	CustAddrLine3 string `cobol:"CUST-ADDR-LINE-3,184,50,alpha"`
	// CustAddrStateCode is the US state code (PIC X(02)).
	CustAddrStateCode string `cobol:"CUST-ADDR-STATE-CD,234,2,alpha"`
	// CustAddrCountryCode is the country code (PIC X(03)).
	CustAddrCountryCode string `cobol:"CUST-ADDR-COUNTRY-CD,236,3,alpha"`
	// CustAddrZip is the ZIP/postal code (PIC X(10)).
	CustAddrZip string `cobol:"CUST-ADDR-ZIP,239,10,alpha"`
	// CustPhoneNum1 is primary phone number (PIC X(15)).
	CustPhoneNum1 string `cobol:"CUST-PHONE-NUM-1,249,15,alpha"`
	// CustPhoneNum2 is secondary phone number (PIC X(15)).
	CustPhoneNum2 string `cobol:"CUST-PHONE-NUM-2,264,15,alpha"`
	// CustSSN is the Social Security Number (PIC 9(09)).
	CustSSN int64 `cobol:"CUST-SSN,279,9,zoned_uint"`
	// CustGovtIssuedID is a government-issued ID number (PIC X(20)).
	CustGovtIssuedID string `cobol:"CUST-GOVT-ISSUED-ID,288,20,alpha"`
	// CustDOB is the date of birth YYYY-MM-DD (PIC X(10)).
	CustDOB string `cobol:"CUST-DOB-YYYY-MM-DD,308,10,alpha"`
	// CustEFTAccountID is the EFT account ID (PIC X(10)).
	CustEFTAccountID string `cobol:"CUST-EFT-ACCOUNT-ID,318,10,alpha"`
	// CustPriCardHolderInd is 'Y' if primary card holder (PIC X(01)).
	CustPriCardHolderInd string `cobol:"CUST-PRI-CARD-HOLDER-IND,328,1,alpha"`
	// CustFICOCreditScore is the FICO credit score (PIC 9(03)).
	CustFICOCreditScore int64 `cobol:"CUST-FICO-CREDIT-SCORE,329,3,zoned_uint"`
}

// CustomerRecordLen is the fixed length of a CUSTOMER-RECORD in bytes.
const CustomerRecordLen = 500

// DecodeCustomerRecord decodes one 500-byte EBCDIC record into a CustomerRecord.
func DecodeCustomerRecord(b []byte) (*CustomerRecord, error) {
	if len(b) < CustomerRecordLen {
		return nil, fmt.Errorf("domain: customer: record too short: got %d, want %d", len(b), CustomerRecordLen)
	}
	b = b[:CustomerRecordLen]

	var r CustomerRecord
	var err error

	if r.CustID, err = cobolfmt.DecodeZonedUint(b[0:9]); err != nil {
		return nil, fmt.Errorf("domain: customer: CUST-ID: %w", err)
	}

	alphaFields := []struct {
		dst    *string
		offset int
		length int
		label  string
	}{
		{&r.CustFirstName, 9, 25, "CUST-FIRST-NAME"},
		{&r.CustMiddleName, 34, 25, "CUST-MIDDLE-NAME"},
		{&r.CustLastName, 59, 25, "CUST-LAST-NAME"},
		{&r.CustAddrLine1, 84, 50, "CUST-ADDR-LINE-1"},
		{&r.CustAddrLine2, 134, 50, "CUST-ADDR-LINE-2"},
		{&r.CustAddrLine3, 184, 50, "CUST-ADDR-LINE-3"},
		{&r.CustAddrStateCode, 234, 2, "CUST-ADDR-STATE-CD"},
		{&r.CustAddrCountryCode, 236, 3, "CUST-ADDR-COUNTRY-CD"},
		{&r.CustAddrZip, 239, 10, "CUST-ADDR-ZIP"},
		{&r.CustPhoneNum1, 249, 15, "CUST-PHONE-NUM-1"},
		{&r.CustPhoneNum2, 264, 15, "CUST-PHONE-NUM-2"},
		{&r.CustGovtIssuedID, 288, 20, "CUST-GOVT-ISSUED-ID"},
		{&r.CustDOB, 308, 10, "CUST-DOB-YYYY-MM-DD"},
		{&r.CustEFTAccountID, 318, 10, "CUST-EFT-ACCOUNT-ID"},
		{&r.CustPriCardHolderInd, 328, 1, "CUST-PRI-CARD-HOLDER-IND"},
	}
	for _, af := range alphaFields {
		*af.dst, err = cobolfmt.DecodeEBCDICTrimmed(b[af.offset : af.offset+af.length])
		if err != nil {
			return nil, fmt.Errorf("domain: customer: %s: %w", af.label, err)
		}
	}

	if r.CustSSN, err = cobolfmt.DecodeZonedUint(b[279:288]); err != nil {
		return nil, fmt.Errorf("domain: customer: CUST-SSN: %w", err)
	}
	if r.CustFICOCreditScore, err = cobolfmt.DecodeZonedUint(b[329:332]); err != nil {
		return nil, fmt.Errorf("domain: customer: CUST-FICO-CREDIT-SCORE: %w", err)
	}

	return &r, nil
}

// Encode serialises a CustomerRecord back to its 500-byte EBCDIC representation.
func (r *CustomerRecord) Encode() ([]byte, error) {
	out := make([]byte, CustomerRecordLen)

	fid, err := cobolfmt.EncodeZonedUint(r.CustID, 9)
	if err != nil {
		return nil, fmt.Errorf("domain: customer: CUST-ID: %w", err)
	}
	copy(out[0:9], fid)

	alphaFields := []struct {
		val    string
		offset int
		length int
		label  string
	}{
		{r.CustFirstName, 9, 25, "CUST-FIRST-NAME"},
		{r.CustMiddleName, 34, 25, "CUST-MIDDLE-NAME"},
		{r.CustLastName, 59, 25, "CUST-LAST-NAME"},
		{r.CustAddrLine1, 84, 50, "CUST-ADDR-LINE-1"},
		{r.CustAddrLine2, 134, 50, "CUST-ADDR-LINE-2"},
		{r.CustAddrLine3, 184, 50, "CUST-ADDR-LINE-3"},
		{r.CustAddrStateCode, 234, 2, "CUST-ADDR-STATE-CD"},
		{r.CustAddrCountryCode, 236, 3, "CUST-ADDR-COUNTRY-CD"},
		{r.CustAddrZip, 239, 10, "CUST-ADDR-ZIP"},
		{r.CustPhoneNum1, 249, 15, "CUST-PHONE-NUM-1"},
		{r.CustPhoneNum2, 264, 15, "CUST-PHONE-NUM-2"},
		{r.CustGovtIssuedID, 288, 20, "CUST-GOVT-ISSUED-ID"},
		{r.CustDOB, 308, 10, "CUST-DOB-YYYY-MM-DD"},
		{r.CustEFTAccountID, 318, 10, "CUST-EFT-ACCOUNT-ID"},
		{r.CustPriCardHolderInd, 328, 1, "CUST-PRI-CARD-HOLDER-IND"},
	}
	for _, af := range alphaFields {
		fb, ferr := cobolfmt.EncodeEBCDIC(cobolfmt.PadRight(af.val, af.length), af.length)
		if ferr != nil {
			return nil, fmt.Errorf("domain: customer: %s: %w", af.label, ferr)
		}
		copy(out[af.offset:af.offset+af.length], fb)
	}

	fssn, err := cobolfmt.EncodeZonedUint(r.CustSSN, 9)
	if err != nil {
		return nil, fmt.Errorf("domain: customer: CUST-SSN: %w", err)
	}
	copy(out[279:288], fssn)

	ffico, err := cobolfmt.EncodeZonedUint(r.CustFICOCreditScore, 3)
	if err != nil {
		return nil, fmt.Errorf("domain: customer: CUST-FICO-CREDIT-SCORE: %w", err)
	}
	copy(out[329:332], ffico)

	copy(out[332:500], cobolfmt.EBCDICSpacePad(168))
	return out, nil
}

// DecodeCustomerRecords reads all CustomerRecords from a flat fixed-length binary file.
func DecodeCustomerRecords(data []byte) ([]*CustomerRecord, error) {
	if len(data)%CustomerRecordLen != 0 {
		return nil, fmt.Errorf("domain: customer: data length %d is not a multiple of %d", len(data), CustomerRecordLen)
	}
	count := len(data) / CustomerRecordLen
	records := make([]*CustomerRecord, 0, count)
	for i := 0; i < count; i++ {
		rec, err := DecodeCustomerRecord(data[i*CustomerRecordLen : (i+1)*CustomerRecordLen])
		if err != nil {
			return nil, fmt.Errorf("domain: customer: record %d: %w", i, err)
		}
		records = append(records, rec)
	}
	return records, nil
}
