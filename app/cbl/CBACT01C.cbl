000100******************************************************************
000200* PROGRAM     : CBACT01C.CBL
000300* Application : CardDemo
000400* Type        : BATCH COBOL Program
000500* FUNCTION    : READ THE ACCOUNT FILE AND WRITE INTO FILES.
000600******************************************************************
000700* Copyright Amazon.com, Inc. or its affiliates.
000800* All Rights Reserved.
000900*
001000* Licensed under the Apache License, Version 2.0 (the "License").
001100* You may not use this file except in compliance with the License.
001200* You may obtain a copy of the License at
001300*
001400*    http://www.apache.org/licenses/LICENSE-2.0
001500*
001600* Unless required by applicable law or agreed to in writing,
001700* software distributed under the License is distributed on an
001800* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
001900* either express or implied. See the License for the specific
002000* language governing permissions and limitations under the License
002100******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBACT01C.
       AUTHOR.        AWS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCTFILE-FILE ASSIGN TO ACCTFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS FD-ACCT-ID
                  FILE STATUS  IS ACCTFILE-STATUS.
      *
           SELECT OUT-FILE ASSIGN TO OUTFILE
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS OUTFILE-STATUS.
      *
           SELECT ARRY-FILE ASSIGN TO ARRYFILE
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS ARRYFILE-STATUS.
      *
           SELECT VBRC-FILE ASSIGN TO VBRCFILE
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS VBRCFILE-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  ACCTFILE-FILE.
       01  FD-ACCTFILE-REC.
           05 FD-ACCT-ID                        PIC 9(11).
           05 FD-ACCT-DATA                      PIC X(289).
       FD OUT-FILE.
       01 OUT-ACCT-REC.
          05  OUT-ACCT-ID                PIC 9(11).
          05  OUT-ACCT-ACTIVE-STATUS     PIC X(01).
          05  OUT-ACCT-CURR-BAL          PIC S9(10)V99.
          05  OUT-ACCT-CREDIT-LIMIT      PIC S9(10)V99.
          05  OUT-ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99.
          05  OUT-ACCT-OPEN-DATE         PIC X(10).
          05  OUT-ACCT-EXPIRAION-DATE    PIC X(10).
          05  OUT-ACCT-REISSUE-DATE      PIC X(10).
          05  OUT-ACCT-CURR-CYC-CREDIT   PIC S9(10)V99.
          05  OUT-ACCT-CURR-CYC-DEBIT    PIC S9(10)V99
                                         USAGE IS COMP-3.
          05  OUT-ACCT-GROUP-ID          PIC X(10).
      *
       FD ARRY-FILE.
       01 ARR-ARRAY-REC.
          05  ARR-ACCT-ID                PIC 9(11).
          05  ARR-ACCT-BAL OCCURS 5  TIMES.
            10  ARR-ACCT-CURR-BAL        PIC S9(10)V99.
            10  ARR-ACCT-CURR-CYC-DEBIT  PIC S9(10)V99
                                         USAGE IS COMP-3.
          05  ARR-FILLER                 PIC X(04).
      *
       FD VBRC-FILE
                  RECORDING MODE IS V
                  RECORD IS VARYING IN SIZE
                  FROM 10 TO 80 DEPENDING
                  ON WS-RECD-LEN.
       01 VBR-REC                        PIC X(80).
       WORKING-STORAGE SECTION.

      ****0************************************************************
       COPY CVACT01Y.
       COPY CODATECN.
       01  ACCTFILE-STATUS.
           05  ACCTFILE-STAT1      PIC X.
           05  ACCTFILE-STAT2      PIC X.
       01  OUTFILE-STATUS.
           05  OUTFILE-STAT1       PIC X.
           05  OUTFILE-STAT2       PIC X.
       01  ARRYFILE-STATUS.
           05  ARRYFILE-STAT1      PIC X.
           05  ARRYFILE-STAT2      PIC X.
       01  VBRCFILE-STATUS.
           05  VBRCFILE-STAT1      PIC X.
           05  VBRCFILE-STAT2      PIC X.

       01  IO-STATUS.
           05  IO-STAT1            PIC X.
           05  IO-STAT2            PIC X.
       01  TWO-BYTES-BINARY        PIC 9(4) BINARY.
       01  TWO-BYTES-ALPHA         REDEFINES TWO-BYTES-BINARY.
           05  TWO-BYTES-LEFT      PIC X.
           05  TWO-BYTES-RIGHT     PIC X.
       01  IO-STATUS-04.
           05  IO-STATUS-0401      PIC 9   VALUE 0.
           05  IO-STATUS-0403      PIC 999 VALUE 0.

       01  APPL-RESULT             PIC S9(9)   COMP.
           88  APPL-AOK            VALUE 0.
           88  APPL-EOF            VALUE 16.

       01  END-OF-FILE             PIC X(01)    VALUE 'N'.
       01  ABCODE                  PIC S9(9) BINARY.
       01  TIMING                  PIC S9(9) BINARY.
       01  WS-RECD-LEN             PIC  9(04).
       01 VBRC-REC1.
          05  VB1-ACCT-ID                PIC 9(11).
          05  VB1-ACCT-ACTIVE-STATUS     PIC X(01).
       01 VBRC-REC2.
          05  VB2-ACCT-ID                PIC 9(11).
          05  VB2-ACCT-CURR-BAL          PIC S9(10)V99.
          05  VB2-ACCT-CREDIT-LIMIT      PIC S9(10)V99.
          05  VB2-ACCT-REISSUE-YYYY      PIC X(04).
       01 WS-ACCT-REISSUE-DATE.
          05  WS-ACCT-REISSUE-YYYY       PIC X(04).
          05  WS-FILLER-1                PIC X(01).
          05  WS-ACCT-REISSUE-MM         PIC X(02).
          05  WS-FILLER-2                PIC X(01).
          05  WS-ACCT-REISSUE-DD         PIC X(02).
       01 WS-REISSUE-DATE REDEFINES WS-ACCT-REISSUE-DATE  PIC X(10).

      *****************************************************************
       PROCEDURE DIVISION.
           DISPLAY 'START OF EXECUTION OF PROGRAM CBACT01C'.
           PERFORM 0000-ACCTFILE-OPEN.
           PERFORM 2000-OUTFILE-OPEN.
           PERFORM 3000-ARRFILE-OPEN.
           PERFORM 4000-VBRFILE-OPEN.

           PERFORM UNTIL END-OF-FILE = 'Y'
               IF  END-OF-FILE = 'N'
                   PERFORM 1000-ACCTFILE-GET-NEXT
                   IF  END-OF-FILE = 'N'
                       DISPLAY ACCOUNT-RECORD
                   END-IF
               END-IF
           END-PERFORM.

           PERFORM 9000-ACCTFILE-CLOSE.

           DISPLAY 'END OF EXECUTION OF PROGRAM CBACT01C'.

           GOBACK.

      *****************************************************************
      * I/O ROUTINES TO ACCESS A KSDS, VSAM DATA SET...               *
      *****************************************************************
       1000-ACCTFILE-GET-NEXT.
           READ ACCTFILE-FILE INTO ACCOUNT-RECORD.
           IF  ACCTFILE-STATUS = '00'
               MOVE 0 TO APPL-RESULT
               INITIALIZE ARR-ARRAY-REC
               PERFORM 1100-DISPLAY-ACCT-RECORD
               PERFORM 1300-POPUL-ACCT-RECORD
               PERFORM 1350-WRITE-ACCT-RECORD
               PERFORM 1400-POPUL-ARRAY-RECORD
               PERFORM 1450-WRITE-ARRY-RECORD
               INITIALIZE VBRC-REC1
               PERFORM 1500-POPUL-VBRC-RECORD
               PERFORM 1550-WRITE-VB1-RECORD
               PERFORM 1575-WRITE-VB2-RECORD
           ELSE
               IF  ACCTFILE-STATUS = '10'
                   MOVE 16 TO APPL-RESULT
               ELSE
                   MOVE 12 TO APPL-RESULT
               END-IF
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               IF  APPL-EOF
                   MOVE 'Y' TO END-OF-FILE
               ELSE
                   DISPLAY 'ERROR READING ACCOUNT FILE'
                   MOVE ACCTFILE-STATUS TO IO-STATUS
                   PERFORM 9910-DISPLAY-IO-STATUS
                   PERFORM 9999-ABEND-PROGRAM
               END-IF
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       1100-DISPLAY-ACCT-RECORD.
           DISPLAY 'ACCT-ID                 :'   ACCT-ID
           DISPLAY 'ACCT-ACTIVE-STATUS      :'   ACCT-ACTIVE-STATUS
           DISPLAY 'ACCT-CURR-BAL           :'   ACCT-CURR-BAL
           DISPLAY 'ACCT-CREDIT-LIMIT       :'   ACCT-CREDIT-LIMIT
           DISPLAY 'ACCT-CASH-CREDIT-LIMIT  :'   ACCT-CASH-CREDIT-LIMIT
           DISPLAY 'ACCT-OPEN-DATE          :'   ACCT-OPEN-DATE
           DISPLAY 'ACCT-EXPIRAION-DATE     :'   ACCT-EXPIRAION-DATE
           DISPLAY 'ACCT-REISSUE-DATE       :'   ACCT-REISSUE-DATE
           DISPLAY 'ACCT-CURR-CYC-CREDIT    :'   ACCT-CURR-CYC-CREDIT
           DISPLAY 'ACCT-CURR-CYC-DEBIT     :'   ACCT-CURR-CYC-DEBIT
           DISPLAY 'ACCT-GROUP-ID           :'   ACCT-GROUP-ID
           DISPLAY '-------------------------------------------------'
           EXIT.
      *---------------------------------------------------------------*
       1300-POPUL-ACCT-RECORD.
           MOVE   ACCT-ID                 TO   OUT-ACCT-ID.
           MOVE   ACCT-ACTIVE-STATUS      TO   OUT-ACCT-ACTIVE-STATUS.
           MOVE   ACCT-CURR-BAL           TO   OUT-ACCT-CURR-BAL.
           MOVE   ACCT-CREDIT-LIMIT       TO   OUT-ACCT-CREDIT-LIMIT.
           MOVE   ACCT-CASH-CREDIT-LIMIT  TO OUT-ACCT-CASH-CREDIT-LIMIT.
           MOVE   ACCT-OPEN-DATE          TO   OUT-ACCT-OPEN-DATE.
           MOVE   ACCT-EXPIRAION-DATE     TO   OUT-ACCT-EXPIRAION-DATE.
           MOVE   ACCT-REISSUE-DATE       TO   CODATECN-INP-DATE
                                               WS-REISSUE-DATE.
           MOVE   '2'                     TO   CODATECN-TYPE.
           MOVE   '2'                     TO   CODATECN-OUTTYPE.

      *---------------------------------------------------------------*
      *CALL ASSEMBLER PROGRAM FOR DATE FORMATTING                     *
      *---------------------------------------------------------------*
           CALL 'COBDATFT'       USING CODATECN-REC.

           MOVE   CODATECN-0UT-DATE       TO   OUT-ACCT-REISSUE-DATE.

           MOVE   ACCT-CURR-CYC-CREDIT    TO   OUT-ACCT-CURR-CYC-CREDIT.
           IF  ACCT-CURR-CYC-DEBIT EQUAL TO ZERO
               MOVE 2525.00         TO   OUT-ACCT-CURR-CYC-DEBIT
           END-IF.
           MOVE   ACCT-GROUP-ID           TO   OUT-ACCT-GROUP-ID.
           EXIT.
      *---------------------------------------------------------------*
       1350-WRITE-ACCT-RECORD.
           WRITE OUT-ACCT-REC.

           IF OUTFILE-STATUS NOT = '00' AND OUTFILE-STATUS NOT = '10'
              DISPLAY 'ACCOUNT FILE WRITE STATUS IS:'  OUTFILE-STATUS
              MOVE OUTFILE-STATUS  TO IO-STATUS
              PERFORM 9910-DISPLAY-IO-STATUS
              PERFORM 9999-ABEND-PROGRAM
           END-IF.
           EXIT.
      *---------------------------------------------------------------*
       1400-POPUL-ARRAY-RECORD.
           MOVE   ACCT-ID         TO   ARR-ACCT-ID.
           MOVE   ACCT-CURR-BAL   TO   ARR-ACCT-CURR-BAL(1).
           MOVE   1005.00         TO   ARR-ACCT-CURR-CYC-DEBIT(1).
           MOVE   ACCT-CURR-BAL   TO   ARR-ACCT-CURR-BAL(2).
           MOVE   1525.00         TO   ARR-ACCT-CURR-CYC-DEBIT(2).
           MOVE   -1025.00        TO   ARR-ACCT-CURR-BAL(3).
           MOVE   -2500.00        TO   ARR-ACCT-CURR-CYC-DEBIT(3).
           EXIT.
      *---------------------------------------------------------------*
       1450-WRITE-ARRY-RECORD.
           WRITE ARR-ARRAY-REC.

           IF ARRYFILE-STATUS NOT = '00'
                        AND ARRYFILE-STATUS NOT = '10'
              DISPLAY 'ACCOUNT FILE WRITE STATUS IS:'
                                        ARRYFILE-STATUS
              MOVE ARRYFILE-STATUS TO IO-STATUS
              PERFORM 9910-DISPLAY-IO-STATUS
              PERFORM 9999-ABEND-PROGRAM
           END-IF.
           EXIT.
      *---------------------------------------------------------------*
       1500-POPUL-VBRC-RECORD.
           MOVE   ACCT-ID            TO VB1-ACCT-ID
                                        VB2-ACCT-ID.
           MOVE   ACCT-ACTIVE-STATUS TO VB1-ACCT-ACTIVE-STATUS.
           MOVE   ACCT-CURR-BAL           TO  VB2-ACCT-CURR-BAL.
           MOVE   ACCT-CREDIT-LIMIT       TO  VB2-ACCT-CREDIT-LIMIT.
           MOVE   WS-ACCT-REISSUE-YYYY    TO  VB2-ACCT-REISSUE-YYYY.
           DISPLAY 'VBRC-REC1:' VBRC-REC1.
           DISPLAY 'VBRC-REC2:' VBRC-REC2.
           EXIT.
      *---------------------------------------------------------------*
       1550-WRITE-VB1-RECORD.
           MOVE 12 TO WS-RECD-LEN.
           MOVE VBRC-REC1 TO VBR-REC(1:WS-RECD-LEN).
           WRITE VBR-REC.

           IF VBRCFILE-STATUS NOT = '00'
                        AND VBRCFILE-STATUS NOT = '10'
              DISPLAY 'ACCOUNT FILE WRITE STATUS IS:'
                                        VBRCFILE-STATUS
              MOVE VBRCFILE-STATUS TO IO-STATUS
              PERFORM 9910-DISPLAY-IO-STATUS
              PERFORM 9999-ABEND-PROGRAM
           END-IF.
           EXIT.
      *---------------------------------------------------------------*
       1575-WRITE-VB2-RECORD.
           MOVE 39 TO WS-RECD-LEN.
           MOVE VBRC-REC2 TO VBR-REC(1:WS-RECD-LEN).
           WRITE VBR-REC.

           IF VBRCFILE-STATUS NOT = '00'
                        AND VBRCFILE-STATUS NOT = '10'
              DISPLAY 'ACCOUNT FILE WRITE STATUS IS:'
                                        VBRCFILE-STATUS
              MOVE VBRCFILE-STATUS TO IO-STATUS
              PERFORM 9910-DISPLAY-IO-STATUS
              PERFORM 9999-ABEND-PROGRAM
           END-IF.
           EXIT.
      *---------------------------------------------------------------*
       0000-ACCTFILE-OPEN.
           MOVE 8 TO APPL-RESULT.
           OPEN INPUT ACCTFILE-FILE
           IF  ACCTFILE-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR OPENING ACCTFILE'
               MOVE ACCTFILE-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.
       2000-OUTFILE-OPEN.
           MOVE 8 TO APPL-RESULT.
           OPEN OUTPUT OUT-FILE
           IF   OUTFILE-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR OPENING OUTFILE'  OUTFILE-STATUS
               MOVE  OUTFILE-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       3000-ARRFILE-OPEN.
           MOVE 8 TO APPL-RESULT.
           OPEN OUTPUT ARRY-FILE
           IF   ARRYFILE-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR OPENING ARRAYFILE'  ARRYFILE-STATUS
               MOVE  ARRYFILE-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       4000-VBRFILE-OPEN.
           MOVE 8 TO APPL-RESULT.
           OPEN OUTPUT VBRC-FILE
           IF   VBRCFILE-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR OPENING VBRC FILE'  VBRCFILE-STATUS
               MOVE  VBRCFILE-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       9000-ACCTFILE-CLOSE.
           ADD 8 TO ZERO GIVING APPL-RESULT.
           CLOSE ACCTFILE-FILE
           IF  ACCTFILE-STATUS = '00'
               SUBTRACT APPL-RESULT FROM APPL-RESULT
           ELSE
               ADD 12 TO ZERO GIVING APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR CLOSING ACCOUNT FILE'
               MOVE ACCTFILE-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.

       9999-ABEND-PROGRAM.
           DISPLAY 'ABENDING PROGRAM'
           MOVE 0 TO TIMING
           MOVE 999 TO ABCODE
           CALL 'CEE3ABD' USING ABCODE, TIMING.

      *****************************************************************
       9910-DISPLAY-IO-STATUS.
           IF  IO-STATUS NOT NUMERIC
           OR  IO-STAT1 = '9'
               MOVE IO-STAT1 TO IO-STATUS-04(1:1)
               MOVE 0        TO TWO-BYTES-BINARY
               MOVE IO-STAT2 TO TWO-BYTES-RIGHT
               MOVE TWO-BYTES-BINARY TO IO-STATUS-0403
               DISPLAY 'FILE STATUS IS: NNNN' IO-STATUS-04
           ELSE
               MOVE '0000' TO IO-STATUS-04
               MOVE IO-STATUS TO IO-STATUS-04(3:2)
               DISPLAY 'FILE STATUS IS: NNNN' IO-STATUS-04
           END-IF
           EXIT.

      *
      * Ver: CardDemo_v2.0-25-gdb72e6b-235 Date: 2025-04-29 11:01:27 CDT
      *
