// Package web contains the CardDemo HTTP handlers and routing (ADR 0003).
//
// Each BMS map becomes one HTTP handler returning html/template output.
// The chi router (github.com/go-chi/chi/v5) provides URL params and middleware
// composition. Handlers depend on service interfaces, not repositories directly.
//
// BMS map → handler subpackage mapping:
//
//	COSGN00.bms  → web/auth     (sign-on / sign-off, RAU-39 ✓ done)
//	COADM01.bms  → web/admin    (admin menu, RAU-39 ✓ done)
//	COMEN01.bms  → web/menu     (main menu, RAU-43)
//	COACTVW.bms  → web/account  (account view, RAU-38)
//	COACTUP.bms  → web/account  (account update, RAU-38)
//	COCRDSL.bms  → web/card     (card select, RAU-40)
//	COCRDLI.bms  → web/card     (card list, RAU-40)
//	COCRDUP.bms  → web/card     (card update, RAU-40)
//	COTRN00.bms  → web/txn      (transaction list, RAU-41)
//	COTRN01.bms  → web/txn      (add transaction, RAU-41)
//	COTRN02.bms  → web/txn      (view transaction, RAU-41)
//	COBIL00.bms  → web/billing  (bill payment, RAU-42)
//	CORPT00.bms  → web/report   (transaction report, RAU-45)
//	COUSR00.bms  → web/user     (user list, RAU-39 ✓ done)
//	COUSR01.bms  → web/user     (add user, RAU-39 ✓ done)
//	COUSR02.bms  → web/user     (update user, RAU-39 ✓ done)
//	COUSR03.bms  → web/user     (delete user, RAU-39 ✓ done)
package web
