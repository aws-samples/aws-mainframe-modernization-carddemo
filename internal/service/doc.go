// Package service contains CardDemo business-logic services.
//
// Services orchestrate domain types and repositories; they hold no SQL and
// no HTTP concerns. Each service maps to one or more COBOL online programs.
//
// COBOL program → service mapping:
//
//	COACTUPC.cbl  → AccountService.Update   (account update, RAU-38)
//	COACTVWC.cbl  → AccountService.View     (account view, RAU-38)
//	COBIL00C.cbl  → BillingService          (bill payment, RAU-42)
//	COCRDLIC.cbl  → CardService.List        (card list, RAU-40)
//	COCRDSLC.cbl  → CardService.Select      (card select, RAU-40)
//	COCRDUPC.cbl  → CardService.Update      (card update, RAU-40)
//	COMEN01C.cbl  → MenuService             (main menu, RAU-43)
//	CORPT00C.cbl  → ReportService           (transaction report, RAU-45)
//	COTRN00C.cbl  → TransactionService.List (transaction list, RAU-41)
//	COTRN01C.cbl  → TransactionService.Add  (add transaction, RAU-41)
//	COTRN02C.cbl  → TransactionService.View (view transaction, RAU-41)
//	COUSR00C.cbl  → UserService.List        (user list, RAU-39)
//	COUSR01C.cbl  → UserService.Add         (add user, RAU-39)
//	COUSR02C.cbl  → UserService.Update      (update user, RAU-39)
//	COUSR03C.cbl  → UserService.Delete      (delete user, RAU-39)
package service
