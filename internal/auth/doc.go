// Package auth implements CardDemo authentication and session management.
//
// It replaces RACF sign-on (COSGN00C.cbl) and the CICS security interface with
// a Go HTTP-session stack: bcrypt password hashing, CSRF protection, rate
// limiting, and an in-memory (stub) / database-backed session store.
//
// COBOL program → auth component mapping:
//
//	COSGN00C.cbl  → Service.SignIn / Service.SignOut
//	COADM01C.cbl  → admin user CRUD (Service.Create/Update/Delete/List via RAU-39)
//	CSUSR01Y.cpy  → domain.UserSec (the security-record struct)
package auth
