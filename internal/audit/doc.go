// Package audit records security-relevant events for CardDemo.
//
// It replaces the SMF (System Management Facilities) audit trail that RACF and
// CICS write implicitly on the mainframe. Events are written to an append-only
// sink; the Sink interface allows swapping in-memory (tests) and database-backed
// (production) implementations without changing callers.
//
// COBOL programs that emit auditable events:
//
//	COSGN00C.cbl  — sign-on / sign-off events
//	COADM01C.cbl  — admin user create/update/delete events
//	COUSR01C.cbl  — user create events
//	COUSR02C.cbl  — user update events
//	COUSR03C.cbl  — user delete events
package audit
