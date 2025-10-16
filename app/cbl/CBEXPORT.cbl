       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBEXPORT.
       AUTHOR.        CARDDEMO TEAM.
      ******************************************************************
      * Program     : CBEXPORT.CBL
      * Application : CardDemo
      * Type        : BATCH COBOL Program
      * Function    : Export Customer Data for Branch Migration
      *               Reads normalized CardDemo files and creates
      *               multi-record export file for data migration
      ******************************************************************
      * Copyright Amazon.com, Inc. or its affiliates.
      * All Rights Reserved.
      *
      * Licensed under the Apache License, Version 2.0 (the "License").
      * You may not use this file except in compliance with the License.
      * You may obtain a copy of the License at
      *
      *    http://www.apache.org/licenses/LICENSE-2.0
      *
      * Unless required by applicable law or agreed to in writing,
      * software distributed under the License is distributed on an
      * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
      * either express or implied. See the License for the specific
      * language governing permissions and limitations under the License
      ******************************************************************
      * Business Use Case: Branch Migration Export
      * - Read complete customer profiles from CardDemo files
      * - Create multi-record export file with different record types
      * - Generate export statistics and processing reports
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-INPUT ASSIGN TO CUSTFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-CUSTOMER-STATUS.
               
           SELECT ACCOUNT-INPUT ASSIGN TO ACCTFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS ACCT-ID
               FILE STATUS IS WS-ACCOUNT-STATUS.
               
           SELECT XREF-INPUT ASSIGN TO XREFFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS XREF-CARD-NUM
               FILE STATUS IS WS-XREF-STATUS.
               
           SELECT TRANSACTION-INPUT ASSIGN TO TRANSACT
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS TRAN-ID
               FILE STATUS IS WS-TRANSACTION-STATUS.
               
           SELECT CARD-INPUT ASSIGN TO CARDFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CARD-NUM
               FILE STATUS IS WS-CARD-STATUS.
               
           SELECT EXPORT-OUTPUT ASSIGN TO EXPFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS EXPORT-SEQUENCE-NUM
               FILE STATUS IS WS-EXPORT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       
       FD  CUSTOMER-INPUT.
       COPY CVCUS01Y.

       FD  ACCOUNT-INPUT.
       COPY CVACT01Y.

       FD  XREF-INPUT.
       COPY CVACT03Y.

       FD  TRANSACTION-INPUT.
       COPY CVTRA05Y.

       FD  CARD-INPUT.
       COPY CVACT02Y.

       FD  EXPORT-OUTPUT
           RECORDING MODE IS F
           RECORD CONTAINS 500 CHARACTERS.
       01  EXPORT-OUTPUT-RECORD                        PIC X(500).

       WORKING-STORAGE SECTION.

       COPY CVEXPORT.

      * File Status Variables
       01  WS-FILE-STATUS-AREA.
           05  WS-CUSTOMER-STATUS                      PIC X(02).
               88  WS-CUSTOMER-EOF                     VALUE '10'.
               88  WS-CUSTOMER-OK                      VALUE '00'.
           05  WS-ACCOUNT-STATUS                       PIC X(02).
               88  WS-ACCOUNT-EOF                      VALUE '10'.
               88  WS-ACCOUNT-OK                       VALUE '00'.
           05  WS-XREF-STATUS                          PIC X(02).
               88  WS-XREF-EOF                         VALUE '10'.
               88  WS-XREF-OK                          VALUE '00'.
           05  WS-TRANSACTION-STATUS                   PIC X(02).
               88  WS-TRANSACTION-EOF                  VALUE '10'.
               88  WS-TRANSACTION-OK                   VALUE '00'.
           05  WS-CARD-STATUS                          PIC X(02).
               88  WS-CARD-EOF                         VALUE '10'.
               88  WS-CARD-OK                          VALUE '00'.
           05  WS-EXPORT-STATUS                        PIC X(02).
               88  WS-EXPORT-OK                        VALUE '00'.

      * Export Control Variables
       01  WS-EXPORT-CONTROL.
           05  WS-EXPORT-DATE                          PIC X(10).
           05  WS-EXPORT-TIME                          PIC X(08).
           05  WS-FORMATTED-TIMESTAMP                  PIC X(26).
           05  WS-SEQUENCE-COUNTER                    PIC 9(09) VALUE 0.

      * Timestamp Variables
       01  WS-TIMESTAMP-FIELDS.
           05  WS-CURRENT-DATE.
               10  WS-CURR-YEAR                        PIC 9(04).
               10  WS-CURR-MONTH                       PIC 9(02).
               10  WS-CURR-DAY                         PIC 9(02).
           05  WS-CURRENT-TIME.
               10  WS-CURR-HOUR                        PIC 9(02).
               10  WS-CURR-MINUTE                      PIC 9(02).
               10  WS-CURR-SECOND                      PIC 9(02).
               10  WS-CURR-HUNDREDTH                   PIC 9(02).

      * Statistics Counters
       01  WS-EXPORT-STATISTICS.
           05  WS-CUSTOMER-RECORDS-EXPORTED           PIC 9(09) VALUE 0.
           05  WS-ACCOUNT-RECORDS-EXPORTED            PIC 9(09) VALUE 0.
           05  WS-XREF-RECORDS-EXPORTED               PIC 9(09) VALUE 0.
           05  WS-TRAN-RECORDS-EXPORTED        PIC 9(09) VALUE 0.
           05  WS-CARD-RECORDS-EXPORTED               PIC 9(09) VALUE 0.
           05  WS-TOTAL-RECORDS-EXPORTED              PIC 9(09) VALUE 0.

       PROCEDURE DIVISION.

      *****************************************************************
       0000-MAIN-PROCESSING.
      *****************************************************************
           PERFORM 1000-INITIALIZE
           PERFORM 2000-EXPORT-CUSTOMERS
           PERFORM 3000-EXPORT-ACCOUNTS
           PERFORM 4000-EXPORT-XREFS
           PERFORM 5000-EXPORT-TRANSACTIONS
           PERFORM 5500-EXPORT-CARDS
           PERFORM 6000-FINALIZE
           GOBACK.

      *****************************************************************
       1000-INITIALIZE.
      *****************************************************************
           DISPLAY 'CBEXPORT: Starting Customer Data Export'
           
           PERFORM 1050-GENERATE-TIMESTAMP
           PERFORM 1100-OPEN-FILES
           
           DISPLAY 'CBEXPORT: Export Date: ' WS-EXPORT-DATE
           DISPLAY 'CBEXPORT: Export Time: ' WS-EXPORT-TIME.

      *****************************************************************
       1050-GENERATE-TIMESTAMP.
      *****************************************************************
      *    Get current date and time
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-TIME FROM TIME
           
      *    Format date as YYYY-MM-DD
           STRING WS-CURR-YEAR '-' WS-CURR-MONTH '-' WS-CURR-DAY
               DELIMITED BY SIZE
               INTO WS-EXPORT-DATE
           END-STRING
           
      *    Format time as HH:MM:SS
           STRING WS-CURR-HOUR ':' WS-CURR-MINUTE ':' WS-CURR-SECOND
               DELIMITED BY SIZE
               INTO WS-EXPORT-TIME
           END-STRING
           
      *    Create 26-character timestamp for export records
           STRING WS-EXPORT-DATE ' ' WS-EXPORT-TIME '.00'
               DELIMITED BY SIZE
               INTO WS-FORMATTED-TIMESTAMP
           END-STRING
           .

      *****************************************************************
       1100-OPEN-FILES.
      *****************************************************************
           OPEN INPUT CUSTOMER-INPUT
           IF NOT WS-CUSTOMER-OK
               DISPLAY 'ERROR: Cannot open CUSTOMER-INPUT, Status: '
                       WS-CUSTOMER-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           
           OPEN INPUT ACCOUNT-INPUT
           IF NOT WS-ACCOUNT-OK
               DISPLAY 'ERROR: Cannot open ACCOUNT-INPUT, Status: '
                       WS-ACCOUNT-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           
           OPEN INPUT XREF-INPUT
           IF NOT WS-XREF-OK
               DISPLAY 'ERROR: Cannot open XREF-INPUT, Status: '
                       WS-XREF-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           
           OPEN INPUT TRANSACTION-INPUT
           IF NOT WS-TRANSACTION-OK
               DISPLAY 'ERROR: Cannot open TRANSACTION-INPUT, Status: '
                       WS-TRANSACTION-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           
           OPEN INPUT CARD-INPUT
           IF NOT WS-CARD-OK
               DISPLAY 'ERROR: Cannot open CARD-INPUT, Status: '
                       WS-CARD-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           
           OPEN OUTPUT EXPORT-OUTPUT
           IF NOT WS-EXPORT-OK
               DISPLAY 'ERROR: Cannot open EXPORT-OUTPUT, Status: '
                       WS-EXPORT-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF.

      *****************************************************************
       2000-EXPORT-CUSTOMERS.
      *****************************************************************
           DISPLAY 'CBEXPORT: Processing customer records'
           
           PERFORM 2100-READ-CUSTOMER-RECORD
           
           PERFORM UNTIL WS-CUSTOMER-EOF
               PERFORM 2200-CREATE-CUSTOMER-EXP-REC                     
               PERFORM 2100-READ-CUSTOMER-RECORD
           END-PERFORM
           
           DISPLAY 'CBEXPORT: Customers exported: ' 
                   WS-CUSTOMER-RECORDS-EXPORTED.

      *****************************************************************
       2100-READ-CUSTOMER-RECORD.
      *****************************************************************
           READ CUSTOMER-INPUT
           
           IF NOT WS-CUSTOMER-OK AND NOT WS-CUSTOMER-EOF
               DISPLAY 'ERROR: Reading CUSTOMER-INPUT, Status: '
                       WS-CUSTOMER-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF.

      *****************************************************************
       2200-CREATE-CUSTOMER-EXP-REC.
      *****************************************************************
           INITIALIZE EXPORT-RECORD
           
      *    Set record type and common fields
           MOVE 'C' TO EXPORT-REC-TYPE
           MOVE WS-FORMATTED-TIMESTAMP TO EXPORT-TIMESTAMP
           ADD 1 TO WS-SEQUENCE-COUNTER
           MOVE WS-SEQUENCE-COUNTER TO EXPORT-SEQUENCE-NUM
           MOVE '0001' TO EXPORT-BRANCH-ID
           MOVE 'NORTH' TO EXPORT-REGION-CODE
           
      *    Map customer fields to export record
           MOVE CUST-ID TO EXP-CUST-ID
           MOVE CUST-FIRST-NAME TO EXP-CUST-FIRST-NAME
           MOVE CUST-MIDDLE-NAME TO EXP-CUST-MIDDLE-NAME
           MOVE CUST-LAST-NAME TO EXP-CUST-LAST-NAME
           MOVE CUST-ADDR-LINE-1 TO EXP-CUST-ADDR-LINE(1)
           MOVE CUST-ADDR-LINE-2 TO EXP-CUST-ADDR-LINE(2)
           MOVE CUST-ADDR-LINE-3 TO EXP-CUST-ADDR-LINE(3)
           MOVE CUST-ADDR-STATE-CD TO EXP-CUST-ADDR-STATE-CD
           MOVE CUST-ADDR-COUNTRY-CD TO EXP-CUST-ADDR-COUNTRY-CD
           MOVE CUST-ADDR-ZIP TO EXP-CUST-ADDR-ZIP
           MOVE CUST-PHONE-NUM-1 TO EXP-CUST-PHONE-NUM(1)
           MOVE CUST-PHONE-NUM-2 TO EXP-CUST-PHONE-NUM(2)
           MOVE CUST-SSN TO EXP-CUST-SSN
           MOVE CUST-GOVT-ISSUED-ID TO EXP-CUST-GOVT-ISSUED-ID
           MOVE CUST-DOB-YYYY-MM-DD TO EXP-CUST-DOB-YYYY-MM-DD
           MOVE CUST-EFT-ACCOUNT-ID TO EXP-CUST-EFT-ACCOUNT-ID
           MOVE CUST-PRI-CARD-HOLDER-IND TO EXP-CUST-PRI-CARD-HOLDER-IND
           MOVE CUST-FICO-CREDIT-SCORE TO EXP-CUST-FICO-CREDIT-SCORE
           
           WRITE EXPORT-OUTPUT-RECORD FROM EXPORT-RECORD
           
           IF NOT WS-EXPORT-OK
               DISPLAY 'ERROR: Writing export record, Status: '
                       WS-EXPORT-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           
           ADD 1 TO WS-CUSTOMER-RECORDS-EXPORTED
           ADD 1 TO WS-TOTAL-RECORDS-EXPORTED.    
  *****************************************************************
       3000-EXPORT-ACCOUNTS.
      *****************************************************************
           DISPLAY 'CBEXPORT: Processing account records'
           
           PERFORM 3100-READ-ACCOUNT-RECORD
           
           PERFORM UNTIL WS-ACCOUNT-EOF
               PERFORM 3200-CREATE-ACCOUNT-EXP-REC                      
               PERFORM 3100-READ-ACCOUNT-RECORD
           END-PERFORM
           
           DISPLAY 'CBEXPORT: Accounts exported: ' 
                   WS-ACCOUNT-RECORDS-EXPORTED.

      *****************************************************************
       3100-READ-ACCOUNT-RECORD.
      *****************************************************************
           READ ACCOUNT-INPUT
           
           IF NOT WS-ACCOUNT-OK AND NOT WS-ACCOUNT-EOF
               DISPLAY 'ERROR: Reading ACCOUNT-INPUT, Status: '
                       WS-ACCOUNT-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF.

      *****************************************************************
       3200-CREATE-ACCOUNT-EXP-REC.
      *****************************************************************
           INITIALIZE EXPORT-RECORD
           
      *    Set record type and common fields
           MOVE 'A' TO EXPORT-REC-TYPE
           MOVE WS-FORMATTED-TIMESTAMP TO EXPORT-TIMESTAMP
           ADD 1 TO WS-SEQUENCE-COUNTER
           MOVE WS-SEQUENCE-COUNTER TO EXPORT-SEQUENCE-NUM
           MOVE '0001' TO EXPORT-BRANCH-ID
           MOVE 'NORTH' TO EXPORT-REGION-CODE
           
      *    Map account fields to export record
           MOVE ACCT-ID TO EXP-ACCT-ID
           MOVE ACCT-ACTIVE-STATUS TO EXP-ACCT-ACTIVE-STATUS
           MOVE ACCT-CURR-BAL TO EXP-ACCT-CURR-BAL
           MOVE ACCT-CREDIT-LIMIT TO EXP-ACCT-CREDIT-LIMIT
           MOVE ACCT-CASH-CREDIT-LIMIT TO EXP-ACCT-CASH-CREDIT-LIMIT
           MOVE ACCT-OPEN-DATE TO EXP-ACCT-OPEN-DATE
           MOVE ACCT-EXPIRAION-DATE TO EXP-ACCT-EXPIRAION-DATE
           MOVE ACCT-REISSUE-DATE TO EXP-ACCT-REISSUE-DATE
           MOVE ACCT-CURR-CYC-CREDIT TO EXP-ACCT-CURR-CYC-CREDIT
           MOVE ACCT-CURR-CYC-DEBIT TO EXP-ACCT-CURR-CYC-DEBIT
           MOVE ACCT-ADDR-ZIP TO EXP-ACCT-ADDR-ZIP
           MOVE ACCT-GROUP-ID TO EXP-ACCT-GROUP-ID
           
           WRITE EXPORT-OUTPUT-RECORD FROM EXPORT-RECORD
           
           IF NOT WS-EXPORT-OK
               DISPLAY 'ERROR: Writing export record, Status: '
                       WS-EXPORT-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           
           ADD 1 TO WS-ACCOUNT-RECORDS-EXPORTED
           ADD 1 TO WS-TOTAL-RECORDS-EXPORTED.

      *****************************************************************
       4000-EXPORT-XREFS.
      *****************************************************************
           DISPLAY 'CBEXPORT: Processing cross-reference records'
           
           PERFORM 4100-READ-XREF-RECORD
           
           PERFORM UNTIL WS-XREF-EOF
               PERFORM 4200-CREATE-XREF-EXPORT-RECORD
               PERFORM 4100-READ-XREF-RECORD
           END-PERFORM
           
           DISPLAY 'CBEXPORT: Cross-references exported: ' 
                   WS-XREF-RECORDS-EXPORTED.

      *****************************************************************
       4100-READ-XREF-RECORD.
      *****************************************************************
           READ XREF-INPUT
           
           IF NOT WS-XREF-OK AND NOT WS-XREF-EOF
               DISPLAY 'ERROR: Reading XREF-INPUT, Status: '
                       WS-XREF-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF.

      *****************************************************************
       4200-CREATE-XREF-EXPORT-RECORD.
      *****************************************************************
           INITIALIZE EXPORT-RECORD
           
      *    Set record type and common fields
           MOVE 'X' TO EXPORT-REC-TYPE
           MOVE WS-FORMATTED-TIMESTAMP TO EXPORT-TIMESTAMP
           ADD 1 TO WS-SEQUENCE-COUNTER
           MOVE WS-SEQUENCE-COUNTER TO EXPORT-SEQUENCE-NUM
           MOVE '0001' TO EXPORT-BRANCH-ID
           MOVE 'NORTH' TO EXPORT-REGION-CODE
           
      *    Map xref fields to export record
           MOVE XREF-CARD-NUM TO EXP-XREF-CARD-NUM
           MOVE XREF-CUST-ID TO EXP-XREF-CUST-ID
           MOVE XREF-ACCT-ID TO EXP-XREF-ACCT-ID
           
           WRITE EXPORT-OUTPUT-RECORD FROM EXPORT-RECORD
           
           IF NOT WS-EXPORT-OK
               DISPLAY 'ERROR: Writing export record, Status: '
                       WS-EXPORT-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           
           ADD 1 TO WS-XREF-RECORDS-EXPORTED
           ADD 1 TO WS-TOTAL-RECORDS-EXPORTED.

      *****************************************************************
       5000-EXPORT-TRANSACTIONS.
      *****************************************************************
           DISPLAY 'CBEXPORT: Processing transaction records'
           
           PERFORM 5100-READ-TRANSACTION-RECORD
           
           PERFORM UNTIL WS-TRANSACTION-EOF
               PERFORM 5200-CREATE-TRAN-EXP-REC                         
               PERFORM 5100-READ-TRANSACTION-RECORD
           END-PERFORM
           
           DISPLAY 'CBEXPORT: Transactions exported: ' 
                   WS-TRAN-RECORDS-EXPORTED.                            

      *****************************************************************
       5100-READ-TRANSACTION-RECORD.
      *****************************************************************
           READ TRANSACTION-INPUT
           
           IF NOT WS-TRANSACTION-OK AND NOT WS-TRANSACTION-EOF
               DISPLAY 'ERROR: Reading TRANSACTION-INPUT, Status: '
                       WS-TRANSACTION-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF.

      *****************************************************************
       5200-CREATE-TRAN-EXP-REC.                                        
      *****************************************************************
           INITIALIZE EXPORT-RECORD
           
      *    Set record type and common fields
           MOVE 'T' TO EXPORT-REC-TYPE
           MOVE WS-FORMATTED-TIMESTAMP TO EXPORT-TIMESTAMP
           ADD 1 TO WS-SEQUENCE-COUNTER
           MOVE WS-SEQUENCE-COUNTER TO EXPORT-SEQUENCE-NUM
           MOVE '0001' TO EXPORT-BRANCH-ID
           MOVE 'NORTH' TO EXPORT-REGION-CODE
           
      *    Map transaction fields to export record
           MOVE TRAN-ID TO EXP-TRAN-ID
           MOVE TRAN-TYPE-CD TO EXP-TRAN-TYPE-CD
           MOVE TRAN-CAT-CD TO EXP-TRAN-CAT-CD
           MOVE TRAN-SOURCE TO EXP-TRAN-SOURCE
           MOVE TRAN-DESC TO EXP-TRAN-DESC
           MOVE TRAN-AMT TO EXP-TRAN-AMT
           MOVE TRAN-MERCHANT-ID TO EXP-TRAN-MERCHANT-ID
           MOVE TRAN-MERCHANT-NAME TO EXP-TRAN-MERCHANT-NAME
           MOVE TRAN-MERCHANT-CITY TO EXP-TRAN-MERCHANT-CITY
           MOVE TRAN-MERCHANT-ZIP TO EXP-TRAN-MERCHANT-ZIP
           MOVE TRAN-CARD-NUM TO EXP-TRAN-CARD-NUM
           MOVE TRAN-ORIG-TS TO EXP-TRAN-ORIG-TS
           MOVE TRAN-PROC-TS TO EXP-TRAN-PROC-TS
           
           WRITE EXPORT-OUTPUT-RECORD FROM EXPORT-RECORD
           
           IF NOT WS-EXPORT-OK
               DISPLAY 'ERROR: Writing export record, Status: '
                       WS-EXPORT-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           
           ADD 1 TO WS-TRAN-RECORDS-EXPORTED                            
           ADD 1 TO WS-TOTAL-RECORDS-EXPORTED.

      *****************************************************************
       5500-EXPORT-CARDS.
      *****************************************************************
           DISPLAY 'CBEXPORT: Processing card records'
           
           PERFORM 5600-READ-CARD-RECORD
           
           PERFORM UNTIL WS-CARD-EOF
               PERFORM 5700-CREATE-CARD-EXPORT-RECORD
               PERFORM 5600-READ-CARD-RECORD
           END-PERFORM
           
           DISPLAY 'CBEXPORT: Cards exported: ' 
                   WS-CARD-RECORDS-EXPORTED.

      *****************************************************************
       5600-READ-CARD-RECORD.
      *****************************************************************
           READ CARD-INPUT
           
           IF NOT WS-CARD-OK AND NOT WS-CARD-EOF
               DISPLAY 'ERROR: Reading CARD-INPUT, Status: '
                       WS-CARD-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF.

      *****************************************************************
       5700-CREATE-CARD-EXPORT-RECORD.
      *****************************************************************
           INITIALIZE EXPORT-RECORD
           
      *    Set record type and common fields
           MOVE 'D' TO EXPORT-REC-TYPE
           MOVE WS-FORMATTED-TIMESTAMP TO EXPORT-TIMESTAMP
           ADD 1 TO WS-SEQUENCE-COUNTER
           MOVE WS-SEQUENCE-COUNTER TO EXPORT-SEQUENCE-NUM
           MOVE '0001' TO EXPORT-BRANCH-ID
           MOVE 'NORTH' TO EXPORT-REGION-CODE
           
      *    Map card fields to export record
           MOVE CARD-NUM TO EXP-CARD-NUM
           MOVE CARD-ACCT-ID TO EXP-CARD-ACCT-ID
           MOVE CARD-CVV-CD TO EXP-CARD-CVV-CD
           MOVE CARD-EMBOSSED-NAME TO EXP-CARD-EMBOSSED-NAME
           MOVE CARD-EXPIRAION-DATE TO EXP-CARD-EXPIRAION-DATE
           MOVE CARD-ACTIVE-STATUS TO EXP-CARD-ACTIVE-STATUS
           
           WRITE EXPORT-OUTPUT-RECORD FROM EXPORT-RECORD
           
           IF NOT WS-EXPORT-OK
               DISPLAY 'ERROR: Writing export record, Status: '
                       WS-EXPORT-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           
           ADD 1 TO WS-CARD-RECORDS-EXPORTED
           ADD 1 TO WS-TOTAL-RECORDS-EXPORTED.

      *****************************************************************
       6000-FINALIZE.
      *****************************************************************
           CLOSE CUSTOMER-INPUT
           CLOSE ACCOUNT-INPUT
           CLOSE XREF-INPUT
           CLOSE TRANSACTION-INPUT
           CLOSE CARD-INPUT
           CLOSE EXPORT-OUTPUT
           
           DISPLAY 'CBEXPORT: Export completed'
           DISPLAY 'CBEXPORT: Customers Exported: ' 
                   WS-CUSTOMER-RECORDS-EXPORTED
           DISPLAY 'CBEXPORT: Accounts Exported: ' 
                   WS-ACCOUNT-RECORDS-EXPORTED
           DISPLAY 'CBEXPORT: XRefs Exported: ' WS-XREF-RECORDS-EXPORTED
           DISPLAY 'CBEXPORT: Transactions Exported: ' 
                   WS-TRAN-RECORDS-EXPORTED
           DISPLAY 'CBEXPORT: Cards Exported: ' WS-CARD-RECORDS-EXPORTED
           DISPLAY 'CBEXPORT: Total Records Exported: ' 
                   WS-TOTAL-RECORDS-EXPORTED.

      *****************************************************************
       9999-ABEND-PROGRAM.
      *****************************************************************
           DISPLAY 'CBEXPORT: ABENDING PROGRAM'
           CALL 'CEE3ABD'.      
      *
      * Ver: CardDemo_v2.0-44-gb6e9c27-254 Date: 2025-10-16 14:07:18 CDT
      *
