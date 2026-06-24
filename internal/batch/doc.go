// Package batch contains the CardDemo batch-job implementations (ADR 0006).
//
// Each JCL job step is replaced by a Go function invoked via the cmd/batch CLI.
// A thin YAML orchestrator (configs/orchestrator.yaml) replaces JCL job-stream
// sequencing; the functions themselves have no JCL-level control-flow logic.
//
// JCL step → batch function mapping:
//
//	POSTTRAN.jcl   (CBTRN01C.cbl)  → PostTransaction     (RAU-44)
//	TRANCATG.jcl   (CBTRN02C.cbl)  → CategorizeTransaction (RAU-44)
//	DALYREJS.jcl   (CBTRN03C.cbl)  → DailyRejects        (RAU-44)
//	ACCTFILE.jcl   (CBACT01C.cbl)  → AccountFile         (RAU-44)
//	INTCALC.jcl    (CBACT02C.cbl)  → InterestCalc        (RAU-44)
//	TCATBALF.jcl   (CBACT03C.cbl)  → TcatBalance         (RAU-44)
//	READACCT.jcl   (CBACT04C.cbl)  → ReadAccount         (RAU-44)
//	CUSTFILE.jcl   (CBCUS01C.cbl)  → CustomerFile        (RAU-44)
//	CREASTMT.JCL   (CBSTM03A.CBL)  → CreateStatement     (RAU-44)
//	REPTFILE.jcl   (CBSTM03B.CBL)  → ReportFile          (RAU-44)
//	CBEXPORT.jcl   (CBEXPORT.cbl)  → Export              (RAU-44)
//	CBIMPORT.jcl   (CBIMPORT.cbl)  → Import              (RAU-44)
//	COMBTRAN.jcl   (orchestration) → CombineTransactions  (RAU-44)
package batch
