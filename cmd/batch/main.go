// Command batch is the CardDemo batch CLI.
// Each subcommand replaces one JCL step in the original job stream.
// RAU-44 (batch layer) wires in the per-step subcommands; this stub
// prints usage so the binary compiles and the build gate passes now.
//
// JCL → subcommand mapping (to be implemented in internal/batch):
//
//	CBTRN01C / POSTTRAN.jcl   → batch post-transaction
//	CBTRN02C / TRANCATG.jcl   → batch categorize-transaction
//	CBTRN03C / DALYREJS.jcl   → batch daily-rejects
//	CBACT01C / ACCTFILE.jcl   → batch account-file
//	CBACT02C / INTCALC.jcl    → batch interest-calc
//	CBACT03C / TCATBALF.jcl   → batch tcat-balance
//	CBACT04C / READACCT.jcl   → batch read-account
//	CBCUS01C / CUSTFILE.jcl   → batch customer-file
//	CBSTM03A / CREASTMT.JCL   → batch create-statement
//	CBSTM03B / REPTFILE.jcl   → batch report-file
//	CBEXPORT / CBEXPORT.jcl   → batch export
//	CBIMPORT / CBIMPORT.jcl   → batch import
package main

import (
	"fmt"
	"os"
)

func main() {
	fmt.Fprintf(os.Stderr, "usage: carddemo-batch <subcommand> [flags]\n\n")
	fmt.Fprintf(os.Stderr, "Subcommands (stub — RAU-44 will implement these):\n")
	fmt.Fprintf(os.Stderr, "  post-transaction       CBTRN01C / POSTTRAN.jcl\n")
	fmt.Fprintf(os.Stderr, "  categorize-transaction CBTRN02C / TRANCATG.jcl\n")
	fmt.Fprintf(os.Stderr, "  daily-rejects          CBTRN03C / DALYREJS.jcl\n")
	fmt.Fprintf(os.Stderr, "  interest-calc          CBACT02C / INTCALC.jcl\n")
	fmt.Fprintf(os.Stderr, "  create-statement       CBSTM03A / CREASTMT.JCL\n")
	fmt.Fprintf(os.Stderr, "  export                 CBEXPORT / CBEXPORT.jcl\n")
	fmt.Fprintf(os.Stderr, "  import                 CBIMPORT / CBIMPORT.jcl\n")
	os.Exit(1)
}
