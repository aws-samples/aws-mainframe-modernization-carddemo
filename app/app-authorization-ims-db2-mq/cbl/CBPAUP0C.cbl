      ******************************************************************
      * Program     : CBPAUP0C.CBL
      * Application : CardDemo - Authorization Module
      * Type        : BATCH COBOL IMS Program
      * Function    : Delete Expired Pending Authoriation Messages
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
       PROGRAM-ID. CBPAUP0C.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
      *
       FILE SECTION.
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'CBPAUP0C'.
         05 CURRENT-DATE               PIC 9(06).
         05 CURRENT-YYDDD              PIC 9(05).
         05 WS-AUTH-DATE               PIC 9(05).
         05 WS-EXPIRY-DAYS             PIC S9(4) COMP.
         05 WS-DAY-DIFF                PIC S9(4) COMP.
         05 IDX                        PIC S9(4) COMP.
         05 WS-CURR-APP-ID             PIC 9(11).
      *
         05 WS-NO-CHKP                 PIC  9(8) VALUE 0.
         05 WS-AUTH-SMRY-PROC-CNT      PIC  9(8) VALUE 0.
         05 WS-TOT-REC-WRITTEN         PIC S9(8) COMP VALUE 0.
         05 WS-NO-SUMRY-READ           PIC S9(8) COMP VALUE 0.
         05 WS-NO-SUMRY-DELETED        PIC S9(8) COMP VALUE 0.
         05 WS-NO-DTL-READ             PIC S9(8) COMP VALUE 0.
         05 WS-NO-DTL-DELETED          PIC S9(8) COMP VALUE 0.
      *
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                         VALUE 'Y'.
           88 ERR-FLG-OFF                        VALUE 'N'.
         05 WS-END-OF-AUTHDB-FLAG      PIC X(01) VALUE 'N'.
           88 END-OF-AUTHDB                      VALUE 'Y'.
           88 NOT-END-OF-AUTHDB                  VALUE 'N'.
         05 WS-MORE-AUTHS-FLAG         PIC X(01) VALUE 'N'.
           88 MORE-AUTHS                         VALUE 'Y'.
           88 NO-MORE-AUTHS                      VALUE 'N'.
         05 WS-QUALIFY-DELETE-FLAG     PIC X(01) VALUE 'N'.
           88 QUALIFIED-FOR-DELETE               VALUE 'Y'.
           88 NOT-QUALIFIED-FOR-DELETE           VALUE 'N'.
         05 WS-INFILE-STATUS           PIC X(02) VALUE SPACES.
         05 WS-CUSTID-STATUS           PIC X(02) VALUE SPACES.
            88 END-OF-FILE                       VALUE '10'.
      *
         05 WK-CHKPT-ID.
            10  FILLER              PIC  X(04) VALUE 'RMAD'.
            10  WK-CHKPT-ID-CTR     PIC  9(04) VALUE ZEROES.
      *
       01 WS-IMS-VARIABLES.
          05 PSB-NAME                        PIC X(8) VALUE 'PSBPAUTB'.
          05 PCB-OFFSET.
             10 PAUT-PCB-NUM                 PIC S9(4) COMP VALUE +2.
          05 IMS-RETURN-CODE                 PIC X(02).
             88 STATUS-OK                    VALUE '  ', 'FW'.
             88 SEGMENT-NOT-FOUND            VALUE 'GE'.
             88 DUPLICATE-SEGMENT-FOUND      VALUE 'II'.
             88 WRONG-PARENTAGE              VALUE 'GP'.
             88 END-OF-DB                    VALUE 'GB'.
             88 DATABASE-UNAVAILABLE         VALUE 'BA'.
             88 PSB-SCHEDULED-MORE-THAN-ONCE VALUE 'TC'.
             88 COULD-NOT-SCHEDULE-PSB       VALUE 'TE'.
             88 RETRY-CONDITION              VALUE 'BA', 'FH', 'TE'.
          05 WS-IMS-PSB-SCHD-FLG             PIC X(1).
             88  IMS-PSB-SCHD                VALUE 'Y'.
             88  IMS-PSB-NOT-SCHD            VALUE 'N'.

      *
       01 PRM-INFO.
          05 P-EXPIRY-DAYS          PIC 9(02).
          05 FILLER                 PIC X(01).
          05 P-CHKP-FREQ            PIC X(05).
          05 FILLER                 PIC X(01).
          05 P-CHKP-DIS-FREQ        PIC X(05).
          05 FILLER                 PIC X(01).
          05 P-DEBUG-FLAG           PIC X(01).
             88 DEBUG-ON            VALUE 'Y'.
             88 DEBUG-OFF           VALUE 'N'.
          05 FILLER                 PIC X(01).
      *
      *
      *----------------------------------------------------------------*
      *  IMS SEGMENT LAYOUT
      *----------------------------------------------------------------*

      *- PENDING AUTHORIZATION SUMMARY SEGMENT - ROOT
       01 PENDING-AUTH-SUMMARY.
       COPY CIPAUSMY.

      *- PENDING AUTHORIZATION DETAILS SEGMENT - CHILD
       01 PENDING-AUTH-DETAILS.
       COPY CIPAUDTY.

      *
      *----------------------------------------------------------------*
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      * PCB MASKS FOLLOW
       01 IO-PCB-MASK               PIC X.
       01 PGM-PCB-MASK              PIC X.
      *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION                  USING IO-PCB-MASK
                                                 PGM-PCB-MASK.
      *----------------------------------------------------------------*
      *
       MAIN-PARA.
      *
           PERFORM 1000-INITIALIZE                THRU 1000-EXIT
      *
           PERFORM 2000-FIND-NEXT-AUTH-SUMMARY    THRU 2000-EXIT

           PERFORM UNTIL ERR-FLG-ON OR END-OF-AUTHDB

              PERFORM 3000-FIND-NEXT-AUTH-DTL     THRU 3000-EXIT

              PERFORM UNTIL NO-MORE-AUTHS
                 PERFORM 4000-CHECK-IF-EXPIRED    THRU 4000-EXIT

                 IF QUALIFIED-FOR-DELETE
                    PERFORM 5000-DELETE-AUTH-DTL  THRU 5000-EXIT
                 END-IF

                 PERFORM 3000-FIND-NEXT-AUTH-DTL  THRU 3000-EXIT
              END-PERFORM

              IF PA-APPROVED-AUTH-CNT <= 0 AND PA-APPROVED-AUTH-CNT <= 0
                 PERFORM 6000-DELETE-AUTH-SUMMARY THRU 6000-EXIT
              END-IF

              IF WS-AUTH-SMRY-PROC-CNT > P-CHKP-FREQ
                 PERFORM 9000-TAKE-CHECKPOINT     THRU 9000-EXIT

                 MOVE 0                         TO WS-AUTH-SMRY-PROC-CNT
              END-IF
              PERFORM 2000-FIND-NEXT-AUTH-SUMMARY THRU 2000-EXIT

           END-PERFORM
      *
           PERFORM 9000-TAKE-CHECKPOINT           THRU 9000-EXIT
      *
           DISPLAY ' '
           DISPLAY '*-------------------------------------*'
           DISPLAY '# TOTAL SUMMARY READ  :' WS-NO-SUMRY-READ
           DISPLAY '# SUMMARY REC DELETED :' WS-NO-SUMRY-DELETED
           DISPLAY '# TOTAL DETAILS READ  :' WS-NO-DTL-READ
           DISPLAY '# DETAILS REC DELETED :' WS-NO-DTL-DELETED
           DISPLAY '*-------------------------------------*'
           DISPLAY ' '
      *
           GOBACK.
      *
      *----------------------------------------------------------------*
       1000-INITIALIZE.
      *----------------------------------------------------------------*
      *
           ACCEPT CURRENT-DATE     FROM DATE
           ACCEPT CURRENT-YYDDD    FROM DAY

           ACCEPT PRM-INFO FROM SYSIN
           DISPLAY 'STARTING PROGRAM CBPAUP0C::'
           DISPLAY '*-------------------------------------*'
           DISPLAY 'CBPAUP0C PARM RECEIVED :' PRM-INFO
           DISPLAY 'TODAYS DATE            :' CURRENT-YYDDD
           DISPLAY ' '

           IF P-EXPIRY-DAYS IS NUMERIC
              MOVE P-EXPIRY-DAYS     TO WS-EXPIRY-DAYS
           ELSE
              MOVE 5                 TO WS-EXPIRY-DAYS
           END-IF
           IF P-CHKP-FREQ = SPACES OR 0 OR LOW-VALUES
              MOVE 5                 TO P-CHKP-FREQ
           END-IF
           IF P-CHKP-DIS-FREQ = SPACES OR 0 OR LOW-VALUES
              MOVE 10                TO P-CHKP-DIS-FREQ
           END-IF
           IF P-DEBUG-FLAG NOT = 'Y'
              MOVE 'N'               TO P-DEBUG-FLAG
           END-IF
           .
      *
       1000-EXIT.
            EXIT.
      *
      *----------------------------------------------------------------*
       2000-FIND-NEXT-AUTH-SUMMARY.
      *----------------------------------------------------------------*
      *
            IF DEBUG-ON
               DISPLAY 'DEBUG: AUTH SMRY READ : ' WS-NO-SUMRY-READ
            END-IF

            EXEC DLI GN USING PCB(PAUT-PCB-NUM)
                 SEGMENT (PAUTSUM0)
                 INTO (PENDING-AUTH-SUMMARY)
            END-EXEC

            EVALUATE DIBSTAT
               WHEN '  '
                    SET NOT-END-OF-AUTHDB TO TRUE
                    ADD 1                 TO WS-NO-SUMRY-READ
                    ADD 1                 TO WS-AUTH-SMRY-PROC-CNT
                    MOVE PA-ACCT-ID       TO WS-CURR-APP-ID
               WHEN 'GB'
                    SET END-OF-AUTHDB     TO TRUE
               WHEN OTHER
                    DISPLAY 'AUTH SUMMARY READ FAILED  :' DIBSTAT
                    DISPLAY 'SUMMARY READ BEFORE ABEND :'
                                                        WS-NO-SUMRY-READ
                    PERFORM 9999-ABEND
            END-EVALUATE
            .
       2000-EXIT.
            EXIT.
      *
      *
      *----------------------------------------------------------------*
       3000-FIND-NEXT-AUTH-DTL.
      *----------------------------------------------------------------*
      *
            IF DEBUG-ON
               DISPLAY 'DEBUG: AUTH DTL READ : ' WS-NO-DTL-READ
            END-IF

            EXEC DLI GNP USING PCB(PAUT-PCB-NUM)
                 SEGMENT (PAUTDTL1)
                 INTO (PENDING-AUTH-DETAILS)
            END-EXEC
            EVALUATE DIBSTAT
               WHEN '  '
                    SET MORE-AUTHS       TO TRUE
                    ADD 1                TO WS-NO-DTL-READ
               WHEN 'GE'
               WHEN 'GB'
                    SET NO-MORE-AUTHS    TO TRUE
               WHEN OTHER
                    DISPLAY 'AUTH DETAIL READ FAILED  :' DIBSTAT
                    DISPLAY 'SUMMARY AUTH APP ID      :' PA-ACCT-ID
                    DISPLAY 'DETAIL READ BEFORE ABEND :' WS-NO-DTL-READ
                    PERFORM 9999-ABEND
            END-EVALUATE
            .
       3000-EXIT.
            EXIT.
      *
      *----------------------------------------------------------------*
       4000-CHECK-IF-EXPIRED.
      *----------------------------------------------------------------*
      *
            COMPUTE WS-AUTH-DATE = 99999 - PA-AUTH-DATE-9C

            COMPUTE WS-DAY-DIFF = CURRENT-YYDDD - WS-AUTH-DATE

            IF WS-DAY-DIFF >= WS-EXPIRY-DAYS
               SET QUALIFIED-FOR-DELETE       TO TRUE

               IF PA-AUTH-RESP-CODE = '00'
                  SUBTRACT 1                  FROM PA-APPROVED-AUTH-CNT
                  SUBTRACT PA-APPROVED-AMT    FROM PA-APPROVED-AUTH-AMT
               ELSE
                  SUBTRACT 1                  FROM PA-DECLINED-AUTH-CNT
                  SUBTRACT PA-TRANSACTION-AMT FROM PA-DECLINED-AUTH-AMT
               END-IF
            ELSE
               SET NOT-QUALIFIED-FOR-DELETE   TO TRUE
            END-IF

            .
       4000-EXIT.
            EXIT.
      *
      *----------------------------------------------------------------*
       5000-DELETE-AUTH-DTL.
      *----------------------------------------------------------------*
      *
            IF DEBUG-ON
               DISPLAY 'DEBUG: AUTH DTL DLET : ' PA-ACCT-ID
            END-IF

            EXEC DLI DLET USING PCB(PAUT-PCB-NUM)
                 SEGMENT (PAUTDTL1)
                 FROM (PENDING-AUTH-DETAILS)
            END-EXEC

            IF DIBSTAT = SPACES
               ADD 1                     TO WS-NO-DTL-DELETED
            ELSE
               DISPLAY 'AUTH DETAIL DELETE FAILED :' DIBSTAT
               DISPLAY 'AUTH APP ID               :' PA-ACCT-ID
               PERFORM 9999-ABEND
            END-IF

            .
       5000-EXIT.
            EXIT.
      *
      *----------------------------------------------------------------*
       6000-DELETE-AUTH-SUMMARY.
      *----------------------------------------------------------------*
      *
            IF DEBUG-ON
               DISPLAY 'DEBUG: AUTH SMRY DLET : ' PA-ACCT-ID
            END-IF

            EXEC DLI DLET USING PCB(PAUT-PCB-NUM)
                 SEGMENT (PAUTSUM0)
                 FROM (PENDING-AUTH-SUMMARY)
            END-EXEC

            IF DIBSTAT = SPACES
               ADD 1                     TO WS-NO-SUMRY-DELETED
            ELSE
               DISPLAY 'AUTH SUMMARY DELETE FAILED :' DIBSTAT
               DISPLAY 'AUTH APP ID                :' PA-ACCT-ID
               PERFORM 9999-ABEND
            END-IF
            .
       6000-EXIT.
            EXIT.
      *
      *----------------------------------------------------------------*
       9000-TAKE-CHECKPOINT.
      *----------------------------------------------------------------*
      *
           EXEC DLI CHKP ID(WK-CHKPT-ID)
           END-EXEC
      *
           IF DIBSTAT = SPACES
              ADD 1                      TO WS-NO-CHKP
              IF WS-NO-CHKP >= P-CHKP-DIS-FREQ
                 MOVE 0                  TO WS-NO-CHKP
                 DISPLAY 'CHKP SUCCESS: AUTH COUNT - ' WS-NO-SUMRY-READ
                      ', APP ID - ' WS-CURR-APP-ID
              END-IF
           ELSE
              DISPLAY 'CHKP FAILED: DIBSTAT - ' DIBSTAT
                      ', REC COUNT - ' WS-NO-SUMRY-READ
                      ', APP ID - ' WS-CURR-APP-ID
              PERFORM 9999-ABEND
           END-IF
      *
            .
       9000-EXIT.
            EXIT.
      *
      *----------------------------------------------------------------*
       9999-ABEND.
      *----------------------------------------------------------------*
      *
           DISPLAY 'CBPAUP0C ABENDING ...'

           MOVE 16 TO RETURN-CODE
           GOBACK.
      *
       9999-EXIT.
            EXIT.
