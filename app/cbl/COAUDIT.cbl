      ******************************************************************
      * Program:     COAUDIT.CBL                                       *
      * Layer:       Business logic                                    *
      * Function:    Audit Trail Subprogram                           *
      * Description: Centralized audit logging for CardDemo           *
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
       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           COAUDIT.

       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01  WS-WORK-AREAS.
           05  WS-RESP-CD                          PIC S9(09) COMP.
           05  WS-REAS-CD                          PIC S9(09) COMP.
           05  WS-AUDIT-FILE-NAME                  PIC X(8) 
                                                   VALUE 'AUDITLOG'.
           05  WS-RECORD-LENGTH                    PIC 9(4) COMP 
                                                   VALUE 565.
           05  WS-CURRENT-DATE-TIME                PIC X(21).
           05  WS-FORMATTED-TIMESTAMP              PIC X(26).

      ******************************************************************
      * Copy audit parameter structure
      ******************************************************************
       COPY CVAUD01Y.

      ******************************************************************
      * Copy audit log record structure
      ******************************************************************
       COPY AUDITLOG.

      ******************************************************************
      * Copy source record structures
      ******************************************************************
       COPY CUSTREC.
       COPY CVACT01Y.
       COPY CVTRA05Y.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
         05  LK-COMMAREA                           PIC X(01)
             OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.

       PROCEDURE DIVISION.

       0000-MAIN-PROCESSING.
           DISPLAY 'COAUDIT: Starting'
           
      *    Get parameters from COMMAREA
           IF EIBCALEN > 0
              MOVE DFHCOMMAREA(1:EIBCALEN) TO AUDIT-PARMS
           ELSE
              DISPLAY 'COAUDIT: No COMMAREA received'
              MOVE 08 TO AUDIT-OUT-RETURN-CODE
              MOVE 'No communication area received' 
                   TO AUDIT-OUT-ERROR-MSG
              GO TO 9000-RETURN
           END-IF
           
           DISPLAY 'AUDIT-PARMS ' AUDIT-PARMS

      *    Initialize output
           MOVE ZEROS TO AUDIT-OUT-RETURN-CODE
           MOVE SPACES TO AUDIT-OUT-ERROR-MSG
           MOVE ZEROS TO AUDIT-OUT-SQLCODE
           MOVE ZEROS TO AUDIT-OUT-RESP
           MOVE ZEROS TO AUDIT-OUT-RESP2

      *    Validate input
           PERFORM 1000-VALIDATE-INPUT
           IF NOT AUDIT-SUCCESS
              GO TO 9000-RETURN
           END-IF

      *    Generate timestamp
           PERFORM 2000-GENERATE-TIMESTAMP
           IF NOT AUDIT-SUCCESS
              GO TO 9000-RETURN
           END-IF

      *    Build audit record
           PERFORM 3000-BUILD-AUDIT-RECORD
           IF NOT AUDIT-SUCCESS AND NOT AUDIT-WARNING
              GO TO 9000-RETURN
           END-IF

      *    Write audit record
           PERFORM 4000-WRITE-AUDIT-RECORD
           .
       9000-RETURN.
           DISPLAY 'COAUDIT: Returning with RC=' 
                   AUDIT-OUT-RETURN-CODE
           DISPLAY 'COAUDIT: Message='
                   AUDIT-OUT-ERROR-MSG

      *    Return parameters via COMMAREA
           IF EIBCALEN > 0
              MOVE AUDIT-PARMS TO DFHCOMMAREA(1:EIBCALEN)
           END-IF
           
           EXEC CICS RETURN
           END-EXEC
           .

       1000-VALIDATE-INPUT.
           DISPLAY 'COAUDIT: Starting 1000-VALIDATE-INPUT'
      *    Validate User ID
           IF AUDIT-IN-USER-ID = SPACES
              MOVE 08 TO AUDIT-OUT-RETURN-CODE
              MOVE 'User ID is required' TO AUDIT-OUT-ERROR-MSG
              DISPLAY 'COAUDIT: 1000-VALIDATE-INPUT - User ID error'
              EXIT PARAGRAPH
           END-IF

      *    Validate Action Type
           IF NOT (AUDIT-IN-INSERT OR AUDIT-IN-UPDATE 
                   OR AUDIT-IN-DELETE)
              MOVE 08 TO AUDIT-OUT-RETURN-CODE
              MOVE 'Invalid Action Type' TO AUDIT-OUT-ERROR-MSG
              DISPLAY 'COAUDIT: 1000-VALIDATE - Action Type error'
              EXIT PARAGRAPH
           END-IF

      *    Validate Log Type
           IF NOT (AUDIT-IN-CUSTOMER OR AUDIT-IN-ACCOUNT 
                   OR AUDIT-IN-TRANSACTION)
              MOVE 08 TO AUDIT-OUT-RETURN-CODE
              MOVE 'Invalid Log Type' TO AUDIT-OUT-ERROR-MSG
              DISPLAY 'COAUDIT: 1000-VALIDATE - Log Type error'
              EXIT PARAGRAPH
           END-IF

      *    Validate Record Length
           IF AUDIT-IN-RECORD-LENGTH = ZEROS OR 
              AUDIT-IN-RECORD-LENGTH > 500
              MOVE 08 TO AUDIT-OUT-RETURN-CODE
              MOVE 'Invalid Record Length' TO AUDIT-OUT-ERROR-MSG
              DISPLAY 'COAUDIT: 1000-VALIDATE - Record Length error'
              EXIT PARAGRAPH
           END-IF

           MOVE 00 TO AUDIT-OUT-RETURN-CODE
           DISPLAY 'COAUDIT: 1000-VALIDATE-INPUT successfully'
           .

       2000-GENERATE-TIMESTAMP.
           DISPLAY 'COAUDIT: Starting 2000-GENERATE-TIMESTAMP'
           
      *    Get current date and time
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME
           
      *    Format as YYYY-MM-DD-HH.MM.SS.NNNNNN
           MOVE WS-CURRENT-DATE-TIME(1:4) 
             TO WS-FORMATTED-TIMESTAMP(1:4)
           MOVE '-' TO WS-FORMATTED-TIMESTAMP(5:1)
           MOVE WS-CURRENT-DATE-TIME(5:2) 
             TO WS-FORMATTED-TIMESTAMP(6:2)
           MOVE '-' TO WS-FORMATTED-TIMESTAMP(8:1)
           MOVE WS-CURRENT-DATE-TIME(7:2) 
             TO WS-FORMATTED-TIMESTAMP(9:2)
           MOVE '-' TO WS-FORMATTED-TIMESTAMP(11:1)
           MOVE WS-CURRENT-DATE-TIME(9:2) 
             TO WS-FORMATTED-TIMESTAMP(12:2)
           MOVE '.' TO WS-FORMATTED-TIMESTAMP(14:1)
           MOVE WS-CURRENT-DATE-TIME(11:2) 
             TO WS-FORMATTED-TIMESTAMP(15:2)
           MOVE '.' TO WS-FORMATTED-TIMESTAMP(17:1)
           MOVE WS-CURRENT-DATE-TIME(13:2) 
             TO WS-FORMATTED-TIMESTAMP(18:2)
           MOVE '.' TO WS-FORMATTED-TIMESTAMP(20:1)
           MOVE WS-CURRENT-DATE-TIME(15:2) 
             TO WS-FORMATTED-TIMESTAMP(21:2)
           MOVE '0000' TO WS-FORMATTED-TIMESTAMP(23:4)
           
           MOVE 00 TO AUDIT-OUT-RETURN-CODE
           DISPLAY 'COAUDIT: 2000-GENERATE-TIMESTAMP completed'
           .

       3000-BUILD-AUDIT-RECORD.
           DISPLAY 'COAUDIT: Starting 3000-BUILD-AUDIT-RECORD'
      *    Initialize audit record
           MOVE SPACES TO AUDIT-LOG-RECORD
           
      *    Set header fields
           MOVE AUDIT-IN-USER-ID TO AUDIT-USER-ID
           MOVE WS-FORMATTED-TIMESTAMP TO AUDIT-TIMESTAMP
           MOVE AUDIT-IN-USER-TYPE TO AUDIT-USER-TYPE
           MOVE AUDIT-IN-ACTION-TYPE TO AUDIT-ACTION-TYPE
           MOVE AUDIT-IN-LOG-TYPE TO AUDIT-LOG-TYPE

      *    Convert record data based on type
           EVALUATE TRUE
              WHEN AUDIT-IN-CUSTOMER
                 MOVE AUDIT-IN-RECORD-DATA TO CUSTOMER-RECORD
                 PERFORM 3100-CONVERT-CUSTOMER
              WHEN AUDIT-IN-ACCOUNT
                 MOVE AUDIT-IN-RECORD-DATA TO ACCOUNT-RECORD
                 PERFORM 3200-CONVERT-ACCOUNT
              WHEN AUDIT-IN-TRANSACTION
                 MOVE AUDIT-IN-RECORD-DATA TO TRAN-RECORD
                 PERFORM 3300-CONVERT-TRANSACTION
           END-EVALUATE
           
           MOVE 00 TO AUDIT-OUT-RETURN-CODE
           DISPLAY 'COAUDIT: 3000-BUILD-AUDIT-RECORD completed'
           .

       3100-CONVERT-CUSTOMER.
           DISPLAY 'COAUDIT: Starting 3100-CONVERT-CUSTOMER'
      *    DEBUG: Display all customer input fields
           DISPLAY 'CUST-ID: ' CUST-ID
           DISPLAY 'CUST-FIRST-NAME: ' CUST-FIRST-NAME
           DISPLAY 'CUST-MIDDLE-NAME: ' CUST-MIDDLE-NAME
           DISPLAY 'CUST-LAST-NAME: ' CUST-LAST-NAME
           DISPLAY 'CUST-ADDR-LINE-1: ' CUST-ADDR-LINE-1
           DISPLAY 'CUST-ADDR-LINE-2: ' CUST-ADDR-LINE-2
           DISPLAY 'CUST-ADDR-LINE-3: ' CUST-ADDR-LINE-3
           DISPLAY 'CUST-ADDR-STATE-CD: ' CUST-ADDR-STATE-CD
           DISPLAY 'CUST-ADDR-COUNTRY-CD: ' CUST-ADDR-COUNTRY-CD
           DISPLAY 'CUST-ADDR-ZIP: ' CUST-ADDR-ZIP
           DISPLAY 'CUST-PHONE-NUM-1: ' CUST-PHONE-NUM-1
           DISPLAY 'CUST-PHONE-NUM-2: ' CUST-PHONE-NUM-2
           DISPLAY 'CUST-SSN: ' CUST-SSN
           DISPLAY 'CUST-GOVT-ISSUED-ID: ' CUST-GOVT-ISSUED-ID
           DISPLAY 'CUST-DOB-YYYYMMDD: ' CUST-DOB-YYYYMMDD
           DISPLAY 'CUST-EFT-ACCOUNT-ID: ' CUST-EFT-ACCOUNT-ID
           DISPLAY 'CUST-PRI-CARD-HOLDER-IND: ' 
                   CUST-PRI-CARD-HOLDER-IND
           DISPLAY 'CUST-FICO-CREDIT-SCORE: ' CUST-FICO-CREDIT-SCORE
      *    Convert customer fields to audit format
           MOVE CUST-ID TO AUDIT-CUST-ID
           MOVE CUST-FIRST-NAME TO AUDIT-CUST-FIRST-NAME
           MOVE CUST-MIDDLE-NAME TO AUDIT-CUST-MIDDLE-NAME
           MOVE CUST-LAST-NAME TO AUDIT-CUST-LAST-NAME
           MOVE CUST-ADDR-LINE-1 TO AUDIT-CUST-ADDR-LINE-1
           MOVE CUST-ADDR-LINE-2 TO AUDIT-CUST-ADDR-LINE-2
           MOVE CUST-ADDR-LINE-3 TO AUDIT-CUST-ADDR-LINE-3
           MOVE CUST-ADDR-STATE-CD TO AUDIT-CUST-ADDR-STATE-CD
           MOVE CUST-ADDR-COUNTRY-CD TO AUDIT-CUST-ADDR-COUNTRY-CD
           MOVE CUST-ADDR-ZIP TO AUDIT-CUST-ADDR-ZIP
           MOVE CUST-PHONE-NUM-1 TO AUDIT-CUST-PHONE-NUM-1
           MOVE CUST-PHONE-NUM-2 TO AUDIT-CUST-PHONE-NUM-2
           MOVE CUST-SSN TO AUDIT-CUST-SSN
           MOVE CUST-GOVT-ISSUED-ID TO AUDIT-CUST-GOVT-ISSUED-ID
           MOVE CUST-DOB-YYYYMMDD TO AUDIT-CUST-DOB-YYYYMMDD
           MOVE CUST-EFT-ACCOUNT-ID TO AUDIT-CUST-EFT-ACCOUNT-ID
           MOVE CUST-PRI-CARD-HOLDER-IND TO AUDIT-CUST-PRI-CARD-IND
           MOVE CUST-FICO-CREDIT-SCORE TO AUDIT-CUST-FICO-SCORE
           DISPLAY 'COAUDIT: 3100-CONVERT-CUSTOMER completed'
           .

       3200-CONVERT-ACCOUNT.
           DISPLAY 'COAUDIT: Starting 3200-CONVERT-ACCOUNT'
      *    DEBUG: Display all account input fields
           DISPLAY 'ACCT-ID: ' ACCT-ID
           DISPLAY 'ACCT-ACTIVE-STATUS: ' ACCT-ACTIVE-STATUS
           DISPLAY 'ACCT-CURR-BAL: ' ACCT-CURR-BAL
           DISPLAY 'ACCT-CREDIT-LIMIT: ' ACCT-CREDIT-LIMIT
           DISPLAY 'ACCT-CASH-CREDIT-LIMIT: ' ACCT-CASH-CREDIT-LIMIT
           DISPLAY 'ACCT-OPEN-DATE: ' ACCT-OPEN-DATE
           DISPLAY 'ACCT-EXPIRAION-DATE: ' ACCT-EXPIRAION-DATE
           DISPLAY 'ACCT-REISSUE-DATE: ' ACCT-REISSUE-DATE
           DISPLAY 'ACCT-CURR-CYC-CREDIT: ' ACCT-CURR-CYC-CREDIT
           DISPLAY 'ACCT-CURR-CYC-DEBIT: ' ACCT-CURR-CYC-DEBIT
           DISPLAY 'ACCT-ADDR-ZIP: ' ACCT-ADDR-ZIP
           DISPLAY 'ACCT-GROUP-ID: ' ACCT-GROUP-ID
      *    Convert account fields to audit format
           MOVE ACCT-ID TO AUDIT-ACCT-ID
           MOVE ACCT-ACTIVE-STATUS TO AUDIT-ACCT-ACTIVE-STATUS
           MOVE ACCT-CURR-BAL TO AUDIT-ACCT-CURR-BAL
           MOVE ACCT-CREDIT-LIMIT TO AUDIT-ACCT-CREDIT-LIMIT
           MOVE ACCT-CASH-CREDIT-LIMIT TO AUDIT-ACCT-CASH-LIMIT
           MOVE ACCT-OPEN-DATE TO AUDIT-ACCT-OPEN-DATE
           MOVE ACCT-EXPIRAION-DATE TO AUDIT-ACCT-EXPIRATION-DATE
           MOVE ACCT-REISSUE-DATE TO AUDIT-ACCT-REISSUE-DATE
           MOVE ACCT-CURR-CYC-CREDIT TO AUDIT-ACCT-CYC-CREDIT
           MOVE ACCT-CURR-CYC-DEBIT TO AUDIT-ACCT-CYC-DEBIT
           MOVE ACCT-ADDR-ZIP TO AUDIT-ACCT-ADDR-ZIP
           MOVE ACCT-GROUP-ID TO AUDIT-ACCT-GROUP-ID
           DISPLAY 'COAUDIT: 3200-CONVERT-ACCOUNT completed'
           .

       3300-CONVERT-TRANSACTION.
           DISPLAY 'COAUDIT: Starting 3300-CONVERT-TRANSACTION'
      *    DEBUG: Display all transaction input fields
           DISPLAY 'TRAN-ID: ' TRAN-ID
           DISPLAY 'TRAN-TYPE-CD: ' TRAN-TYPE-CD
           DISPLAY 'TRAN-CAT-CD: ' TRAN-CAT-CD
           DISPLAY 'TRAN-SOURCE: ' TRAN-SOURCE
           DISPLAY 'TRAN-DESC: ' TRAN-DESC
           DISPLAY 'TRAN-AMT: ' TRAN-AMT
           DISPLAY 'TRAN-MERCHANT-ID: ' TRAN-MERCHANT-ID
           DISPLAY 'TRAN-MERCHANT-NAME: ' TRAN-MERCHANT-NAME
           DISPLAY 'TRAN-MERCHANT-CITY: ' TRAN-MERCHANT-CITY
           DISPLAY 'TRAN-MERCHANT-ZIP: ' TRAN-MERCHANT-ZIP
           DISPLAY 'TRAN-CARD-NUM: ' TRAN-CARD-NUM
           DISPLAY 'TRAN-ORIG-TS: ' TRAN-ORIG-TS
           DISPLAY 'TRAN-PROC-TS: ' TRAN-PROC-TS
      *    Convert transaction fields to audit format
           MOVE TRAN-ID TO AUDIT-TRAN-ID
           MOVE TRAN-TYPE-CD TO AUDIT-TRAN-TYPE-CD
           MOVE TRAN-CAT-CD TO AUDIT-TRAN-CAT-CD
           MOVE TRAN-SOURCE TO AUDIT-TRAN-SOURCE
           MOVE TRAN-DESC TO AUDIT-TRAN-DESC
           MOVE TRAN-AMT TO AUDIT-TRAN-AMT
           MOVE TRAN-MERCHANT-ID TO AUDIT-TRAN-MERCHANT-ID
           MOVE TRAN-MERCHANT-NAME TO AUDIT-TRAN-MERCHANT-NAME
           MOVE TRAN-MERCHANT-CITY TO AUDIT-TRAN-MERCHANT-CITY
           MOVE TRAN-MERCHANT-ZIP TO AUDIT-TRAN-MERCHANT-ZIP
           MOVE TRAN-CARD-NUM TO AUDIT-TRAN-CARD-NUM
           MOVE TRAN-ORIG-TS TO AUDIT-TRAN-ORIG-TS
           MOVE TRAN-PROC-TS TO AUDIT-TRAN-PROC-TS
           DISPLAY 'COAUDIT: 3300-CONVERT-TRANSACTION completed'
           .

       4000-WRITE-AUDIT-RECORD.
           DISPLAY 'COAUDIT: Starting 4000-WRITE-AUDIT-RECORD'
      *    Validate key fields
           IF AUDIT-LOG-TYPE = SPACES OR AUDIT-TIMESTAMP = SPACES
              OR AUDIT-USER-ID = SPACES
              MOVE 08 TO AUDIT-OUT-RETURN-CODE
              MOVE 'Missing key fields' TO AUDIT-OUT-ERROR-MSG
              DISPLAY 'COAUDIT: 4000-WRITE - Missing key fields'
              EXIT PARAGRAPH
           END-IF

      *    Write to VSAM file
           EXEC CICS WRITE
                DATASET(WS-AUDIT-FILE-NAME)
                FROM(AUDIT-LOG-RECORD)
                LENGTH(WS-RECORD-LENGTH)
                RIDFLD(AUDIT-LOG-RECORD)
                KEYLENGTH(35)
                RESP(WS-RESP-CD)
                RESP2(WS-REAS-CD)
           END-EXEC

      *    Set response codes
           MOVE WS-RESP-CD TO AUDIT-OUT-RESP
           MOVE WS-REAS-CD TO AUDIT-OUT-RESP2

      *    Evaluate result
           EVALUATE WS-RESP-CD
              WHEN DFHRESP(NORMAL)
                 MOVE 00 TO AUDIT-OUT-RETURN-CODE
                 MOVE 'Audit record written successfully' 
                      TO AUDIT-OUT-ERROR-MSG
              WHEN DFHRESP(DUPREC)
                 MOVE 04 TO AUDIT-OUT-RETURN-CODE
                 MOVE 'Duplicate audit record' TO AUDIT-OUT-ERROR-MSG
              WHEN OTHER
                 MOVE 08 TO AUDIT-OUT-RETURN-CODE
                 MOVE 'Audit write failed' TO AUDIT-OUT-ERROR-MSG
           END-EVALUATE
           DISPLAY 'COAUDIT: 4000-WRITE-AUDIT-RECORD completed'
           .

      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:35 CDT
      *