package domain

import (
	"fmt"

	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/cobolfmt"
)

// UserSecRecord is the raw EBCDIC layout of SEC-USER-DATA (CSUSR01Y.cpy, 80 bytes).
// This is distinct from UserSec (the domain model) which uses bcrypt for the password.
//
// COBOL layout:
//
//	05 SEC-USR-ID     PIC X(08)  offset  0 len  8
//	05 SEC-USR-FNAME  PIC X(20)  offset  8 len 20
//	05 SEC-USR-LNAME  PIC X(20)  offset 28 len 20
//	05 SEC-USR-PWD    PIC X(08)  offset 48 len  8  (plaintext on mainframe)
//	05 SEC-USR-TYPE   PIC X(01)  offset 56 len  1
//	05 SEC-USR-FILLER PIC X(23)  offset 57 len 23
type UserSecRecord struct {
	UserID    string `cobol:"SEC-USR-ID,0,8,alpha"`
	FirstName string `cobol:"SEC-USR-FNAME,8,20,alpha"`
	LastName  string `cobol:"SEC-USR-LNAME,28,20,alpha"`
	Password  string `cobol:"SEC-USR-PWD,48,8,alpha"`
	UserType  string `cobol:"SEC-USR-TYPE,56,1,alpha"`
}

// UserSecRecordLen is the fixed length of a SEC-USER-DATA record in bytes.
const UserSecRecordLen = 80

// DecodeUserSecRecord decodes one 80-byte EBCDIC record into a UserSecRecord.
func DecodeUserSecRecord(b []byte) (*UserSecRecord, error) {
	if len(b) < UserSecRecordLen {
		return nil, fmt.Errorf("domain: usersec: record too short: got %d, want %d", len(b), UserSecRecordLen)
	}
	b = b[:UserSecRecordLen]

	var r UserSecRecord
	var err error

	fields := []struct {
		dst    *string
		offset int
		length int
		label  string
	}{
		{&r.UserID, 0, 8, "SEC-USR-ID"},
		{&r.FirstName, 8, 20, "SEC-USR-FNAME"},
		{&r.LastName, 28, 20, "SEC-USR-LNAME"},
		{&r.Password, 48, 8, "SEC-USR-PWD"},
		{&r.UserType, 56, 1, "SEC-USR-TYPE"},
	}
	for _, f := range fields {
		*f.dst, err = cobolfmt.DecodeEBCDICTrimmed(b[f.offset : f.offset+f.length])
		if err != nil {
			return nil, fmt.Errorf("domain: usersec: %s: %w", f.label, err)
		}
	}

	return &r, nil
}

// Encode serialises a UserSecRecord back to its 80-byte EBCDIC representation.
func (r *UserSecRecord) Encode() ([]byte, error) {
	out := make([]byte, UserSecRecordLen)

	fields := []struct {
		val    string
		offset int
		length int
		label  string
	}{
		{r.UserID, 0, 8, "SEC-USR-ID"},
		{r.FirstName, 8, 20, "SEC-USR-FNAME"},
		{r.LastName, 28, 20, "SEC-USR-LNAME"},
		{r.Password, 48, 8, "SEC-USR-PWD"},
		{r.UserType, 56, 1, "SEC-USR-TYPE"},
	}
	for _, f := range fields {
		fb, ferr := cobolfmt.EncodeEBCDIC(cobolfmt.PadRight(f.val, f.length), f.length)
		if ferr != nil {
			return nil, fmt.Errorf("domain: usersec: %s: %w", f.label, ferr)
		}
		copy(out[f.offset:f.offset+f.length], fb)
	}

	copy(out[57:80], cobolfmt.EBCDICSpacePad(23))
	return out, nil
}

// DecodeUserSecRecords reads all UserSecRecords from a flat fixed-length binary file.
func DecodeUserSecRecords(data []byte) ([]*UserSecRecord, error) {
	if len(data)%UserSecRecordLen != 0 {
		return nil, fmt.Errorf("domain: usersec: data length %d is not a multiple of %d", len(data), UserSecRecordLen)
	}
	count := len(data) / UserSecRecordLen
	records := make([]*UserSecRecord, 0, count)
	for i := 0; i < count; i++ {
		rec, err := DecodeUserSecRecord(data[i*UserSecRecordLen : (i+1)*UserSecRecordLen])
		if err != nil {
			return nil, fmt.Errorf("domain: usersec: record %d: %w", i, err)
		}
		records = append(records, rec)
	}
	return records, nil
}
