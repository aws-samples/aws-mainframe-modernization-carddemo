      ******************************************************************
      * CVEXPORT.cpy - CardDemo Multi-Record Export Layout
      * This copybook defines the structure for export records
      * stored in the sequential export file for branch migration
      * Total Record Length: 500 bytes
      * Storage Optimization: COMP/COMP-3 fields for numeric data
      * Enhancement: Multiple record types with REDEFINES structure
      ******************************************************************
       01  EXPORT-RECORD.
           05  EXPORT-REC-TYPE                         PIC X(1).
           05  EXPORT-TIMESTAMP                        PIC X(26).
           05  EXPORT-TIMESTAMP-R REDEFINES EXPORT-TIMESTAMP.
               10  EXPORT-DATE                         PIC X(10).
               10  EXPORT-DATE-TIME-SEP                PIC X(1).
               10  EXPORT-TIME                         PIC X(15).
           05  EXPORT-SEQUENCE-NUM                     PIC 9(9) COMP.
           05  EXPORT-BRANCH-ID                        PIC X(4).
           05  EXPORT-REGION-CODE                      PIC X(5).
           05  EXPORT-RECORD-DATA                      PIC X(460).

      ******************************************************************
      * Customer Record Structure - From CVCUS01Y.cpy
      ******************************************************************
           05  EXPORT-CUSTOMER-DATA REDEFINES EXPORT-RECORD-DATA.
               10  EXP-CUST-ID                     PIC 9(09) COMP.
               10  EXP-CUST-FIRST-NAME             PIC X(25).
               10  EXP-CUST-MIDDLE-NAME            PIC X(25).
               10  EXP-CUST-LAST-NAME              PIC X(25).
               10  EXP-CUST-ADDR-LINES OCCURS 3 TIMES.
                   15  EXP-CUST-ADDR-LINE          PIC X(50).
               10  EXP-CUST-ADDR-STATE-CD          PIC X(02).
               10  EXP-CUST-ADDR-COUNTRY-CD        PIC X(03).
               10  EXP-CUST-ADDR-ZIP               PIC X(10).
               10  EXP-CUST-PHONE-NUMS OCCURS 2 TIMES.
                   15  EXP-CUST-PHONE-NUM          PIC X(15).
               10  EXP-CUST-SSN                    PIC 9(09).
               10  EXP-CUST-GOVT-ISSUED-ID         PIC X(20).
               10  EXP-CUST-DOB-YYYY-MM-DD         PIC X(10).
               10  EXP-CUST-EFT-ACCOUNT-ID         PIC X(10).
               10  EXP-CUST-PRI-CARD-HOLDER-IND    PIC X(01).
               10  EXP-CUST-FICO-CREDIT-SCORE      PIC 9(03) COMP-3.
               10  FILLER                          PIC X(134).

      ******************************************************************
      * Account Record Structure - From CVACT01Y.cpy
      ******************************************************************
           05  EXPORT-ACCOUNT-DATA REDEFINES EXPORT-RECORD-DATA.
               10  EXP-ACCT-ID                     PIC 9(11).
               10  EXP-ACCT-ACTIVE-STATUS          PIC X(01).
               10  EXP-ACCT-CURR-BAL               PIC S9(10)V99 COMP-3.
               10  EXP-ACCT-CREDIT-LIMIT           PIC S9(10)V99.
               10  EXP-ACCT-CASH-CREDIT-LIMIT      PIC S9(10)V99 COMP-3.
               10  EXP-ACCT-OPEN-DATE              PIC X(10).
               10  EXP-ACCT-EXPIRAION-DATE         PIC X(10).
               10  EXP-ACCT-REISSUE-DATE           PIC X(10).
               10  EXP-ACCT-CURR-CYC-CREDIT        PIC S9(10)V99.
               10  EXP-ACCT-CURR-CYC-DEBIT         PIC S9(10)V99 COMP.
               10  EXP-ACCT-ADDR-ZIP               PIC X(10).
               10  EXP-ACCT-GROUP-ID               PIC X(10).
               10  FILLER                          PIC X(352).

      ******************************************************************
      * Transaction Record Structure - From CVTRA05Y.cpy
      ******************************************************************
           05  EXPORT-TRANSACTION-DATA REDEFINES EXPORT-RECORD-DATA.
               10  EXP-TRAN-ID                     PIC X(16).
               10  EXP-TRAN-TYPE-CD                PIC X(02).
               10  EXP-TRAN-CAT-CD                 PIC 9(04).
               10  EXP-TRAN-SOURCE                 PIC X(10).
               10  EXP-TRAN-DESC                   PIC X(100).
               10  EXP-TRAN-AMT                    PIC S9(09)V99 COMP-3.
               10  EXP-TRAN-MERCHANT-ID            PIC 9(09) COMP.
               10  EXP-TRAN-MERCHANT-NAME          PIC X(50).
               10  EXP-TRAN-MERCHANT-CITY          PIC X(50).
               10  EXP-TRAN-MERCHANT-ZIP           PIC X(10).
               10  EXP-TRAN-CARD-NUM               PIC X(16).
               10  EXP-TRAN-ORIG-TS                PIC X(26).
               10  EXP-TRAN-PROC-TS                PIC X(26).
               10  FILLER                          PIC X(140).

      ******************************************************************
      * Card Cross-Reference Structure - From CVACT03Y.cpy
      ******************************************************************
           05  EXPORT-CARD-XREF-DATA REDEFINES EXPORT-RECORD-DATA.
               10  EXP-XREF-CARD-NUM               PIC X(16).
               10  EXP-XREF-CUST-ID                PIC 9(09).
               10  EXP-XREF-ACCT-ID                PIC 9(11) COMP.
               10  FILLER                          PIC X(427).

      ******************************************************************
      * Card Record Structure - From CVACT02Y.cpy
      ******************************************************************
           05  EXPORT-CARD-DATA REDEFINES EXPORT-RECORD-DATA.
               10  EXP-CARD-NUM                    PIC X(16).
               10  EXP-CARD-ACCT-ID                PIC 9(11) COMP.
               10  EXP-CARD-CVV-CD                 PIC 9(03) COMP.
               10  EXP-CARD-EMBOSSED-NAME          PIC X(50).
               10  EXP-CARD-EXPIRAION-DATE         PIC X(10).
               10  EXP-CARD-ACTIVE-STATUS          PIC X(01).
               10  FILLER                          PIC X(373).      
      *
      * Ver: CardDemo_v2.0-44-gb6e9c27-254 Date: 2025-10-16 14:07:18 CDT
      *
