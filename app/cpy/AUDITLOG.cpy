      ******************************************************************
      * AUDITLOG.cpy - Audit Trail Record Structure                   
      * This copybook defines the structure for audit log records     
      * stored in the VSAM audit file                                  
      * Total Record Length: 565 bytes                                 
      * Storage Optimization: COMP-3 fields for numeric data           
      * Enhancement: Embedded source file structures for documentation 
      ******************************************************************
       01  AUDIT-LOG-RECORD.
           05  AUDIT-LOG-TYPE            PIC X(1).
               88  AUDIT-CUSTOMER        VALUE 'C'.
               88  AUDIT-ACCOUNT         VALUE 'A'.
               88  AUDIT-TRANSACTION     VALUE 'T'.
           05  AUDIT-TIMESTAMP           PIC X(26).
           05  AUDIT-TIMESTAMP-R REDEFINES AUDIT-TIMESTAMP.
               10  AUDIT-DATE            PIC X(10).
               10  AUDIT-DATE-TIME-SEP   PIC X(1).
               10  AUDIT-TIME            PIC X(15).
           05  AUDIT-USER-ID             PIC X(8).
           05  AUDIT-USER-TYPE           PIC X(1).
           05  AUDIT-ACTION-TYPE         PIC X(1).
               88  AUDIT-INSERT          VALUE 'I'.
               88  AUDIT-UPDATE          VALUE 'U'. 
               88  AUDIT-DELETE          VALUE 'D'.
           05  AUDIT-FILLER              PIC X(28).
           05  AUDIT-RECORD.
               10 AUDIT-RECORD-DATA                    PIC X(500).
      ******************************************************************
      * Customer Record Structure - From CUSTREC.cpy      
      ******************************************************************
               10  AUDIT-CUSTOMER-DATA REDEFINES AUDIT-RECORD-DATA.
                   15  AUDIT-CUST-ID                   PIC 9(09) COMP.
                   15  AUDIT-CUST-FIRST-NAME           PIC X(25).
                   15  AUDIT-CUST-MIDDLE-NAME          PIC X(25).
                   15  AUDIT-CUST-LAST-NAME            PIC X(25).
                   15  AUDIT-CUST-ADDR-LINE-1          PIC X(50).
                   15  AUDIT-CUST-ADDR-LINE-2          PIC X(50).
                   15  AUDIT-CUST-ADDR-LINE-3          PIC X(50).
                   15  AUDIT-CUST-ADDR-STATE-CD        PIC X(02).
                   15  AUDIT-CUST-ADDR-COUNTRY-CD      PIC X(03).
                   15  AUDIT-CUST-ADDR-ZIP             PIC X(10).
                   15  AUDIT-CUST-PHONE-NUM-1          PIC X(15).
                   15  AUDIT-CUST-PHONE-NUM-2          PIC X(15).
                   15  AUDIT-CUST-SSN                  PIC 9(09).
                   15  AUDIT-CUST-GOVT-ISSUED-ID       PIC X(20).
                   15  AUDIT-CUST-DOB-YYYYMMDD         PIC X(10).
                   15  AUDIT-CUST-EFT-ACCOUNT-ID       PIC X(10).
                   15  AUDIT-CUST-PRI-CARD-IND         PIC X(01).
                   15  AUDIT-CUST-FICO-SCORE           PIC 9(03) COMP-3.
                   15  AUDIT-CUST-FILLER               PIC X(174).
      ******************************************************************
      * Account Record Structure - From CVACT01Y.cpy      
      ******************************************************************
               10  AUDIT-ACCOUNT-DATA REDEFINES AUDIT-RECORD-DATA.
                   15  AUDIT-ACCT-ID                   PIC 9(11) COMP.
                   15  AUDIT-ACCT-ACTIVE-STATUS        PIC X(01).
                   15  AUDIT-ACCT-CURR-BAL             PIC S9(10)V99 
                                                        COMP-3.
                   15  AUDIT-ACCT-CREDIT-LIMIT         PIC S9(10)V99 
                                                        COMP-3.
                   15  AUDIT-ACCT-CASH-LIMIT           PIC S9(10)V99 
                                                        COMP-3.
                   15  AUDIT-ACCT-OPEN-DATE            PIC X(10).
                   15  AUDIT-ACCT-EXPIRATION-DATE      PIC X(10).
                   15  AUDIT-ACCT-REISSUE-DATE         PIC X(10).
                   15  AUDIT-ACCT-CYC-CREDIT           PIC S9(10)V99 
                                                        COMP-3.
                   15  AUDIT-ACCT-CYC-DEBIT            PIC S9(10)V99 
                                                        COMP-3.
                   15  AUDIT-ACCT-ADDR-ZIP             PIC X(10).
                   15  AUDIT-ACCT-GROUP-ID             PIC X(10).
                   15  AUDIT-ACCT-FILLER               PIC X(396).
      ******************************************************************
      * Transaction Record Structure - From CVTRA05Y.cpy  
      ******************************************************************
               10  AUDIT-TRANSACTION-DATA REDEFINES AUDIT-RECORD-DATA.
                   15  AUDIT-TRAN-ID                   PIC X(16).
                   15  AUDIT-TRAN-TYPE-CD              PIC X(02).
                   15  AUDIT-TRAN-CAT-CD               PIC 9(04) COMP.
                   15  AUDIT-TRAN-SOURCE               PIC X(10).
                   15  AUDIT-TRAN-DESC                 PIC X(100).
                   15  AUDIT-TRAN-AMT                  PIC S9(09)V99 
                                                        COMP-3.
                   15  AUDIT-TRAN-MERCHANT-ID          PIC 9(09) COMP.
                   15  AUDIT-TRAN-MERCHANT-NAME        PIC X(50).
                   15  AUDIT-TRAN-MERCHANT-CITY        PIC X(50).
                   15  AUDIT-TRAN-MERCHANT-ZIP         PIC X(10).
                   15  AUDIT-TRAN-CARD-NUM             PIC X(16).
                   15  AUDIT-TRAN-ORIG-TS              PIC X(26).
                   15  AUDIT-TRAN-PROC-TS              PIC X(26).
                   15  AUDIT-TRAN-FILLER               PIC X(180).
