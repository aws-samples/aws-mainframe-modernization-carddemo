      ******************************************************************
      * Program     : COPAUS1C.CBL
      * Application : CardDemo - Authorization Module
      * Type        : CICS COBOL IMS BMS Program
      * Function    : Detail View of Authorization Message
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
       PROGRAM-ID. COPAUS1C.                                                    
       AUTHOR.     AWS.                                                         
                                                                                
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
                                                                                
       DATA DIVISION.                                                           
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01 WS-VARIABLES.                                                         
         05 WS-PGM-AUTH-DTL            PIC X(08) VALUE 'COPAUS1C'.              
         05 WS-PGM-AUTH-SMRY           PIC X(08) VALUE 'COPAUS0C'.              
         05 WS-PGM-AUTH-FRAUD          PIC X(08) VALUE 'COPAUS2C'.              
         05 WS-CICS-TRANID             PIC X(04) VALUE 'CPVD'.                  
         05 WS-MESSAGE                 PIC X(80) VALUE SPACES.                  
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.                     
           88 ERR-FLG-ON                         VALUE 'Y'.                     
           88 ERR-FLG-OFF                        VALUE 'N'.                     
         05 WS-AUTHS-EOF               PIC X(01) VALUE 'N'.                     
           88 AUTHS-EOF                          VALUE 'Y'.                     
           88 AUTHS-NOT-EOF                      VALUE 'N'.                     
         05 WS-SEND-ERASE-FLG          PIC X(01) VALUE 'Y'.                     
           88 SEND-ERASE-YES                     VALUE 'Y'.                     
           88 SEND-ERASE-NO                      VALUE 'N'.                     
         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.             
         05 WS-REAS-CD                 PIC S9(09) COMP VALUE ZEROS.             
                                                                                
         05 WS-ACCT-ID                 PIC  9(11).                              
         05 WS-AUTH-KEY                PIC  X(08).                              
         05 WS-AUTH-AMT                PIC -zzzzzzz9.99.                        
         05 WS-AUTH-DATE               PIC X(08) VALUE '00/00/00'.              
         05 WS-AUTH-TIME               PIC X(08) VALUE '00:00:00'.              
                                                                                
       01 WS-TABLES.
          05 WS-DECLINE-REASON-TABLE.
             10   PIC X(20) VALUE '0000APPROVED'.
             10   PIC X(20) VALUE '3100INVALID CARD'.
             10   PIC X(20) VALUE '4100INSUFFICNT FUND'.
             10   PIC X(20) VALUE '4200CARD NOT ACTIVE'.
             10   PIC X(20) VALUE '4300ACCOUNT CLOSED'.
             10   PIC X(20) VALUE '4400EXCED DAILY LMT'.
             10   PIC X(20) VALUE '5100CARD FRAUD'.
             10   PIC X(20) VALUE '5200MERCHANT FRAUD'.
             10   PIC X(20) VALUE '5300LOST CARD'.
             10   PIC X(20) VALUE '9000UNKNOWN'.
          05 WS-DECLINE-REASON-TAB REDEFINES WS-DECLINE-REASON-TABLE
                                OCCURS 10 TIMES
                                ASCENDING KEY IS DECL-CODE
                                INDEXED BY WS-DECL-RSN-IDX.
             10 DECL-CODE                PIC X(4).
             10 DECL-DESC                PIC X(16).

       01 WS-IMS-VARIABLES.
          05 PSB-NAME                        PIC X(8) VALUE 'PSBPAUTB'.
          05 PCB-OFFSET.
             10 PAUT-PCB-NUM                 PIC S9(4) COMP VALUE +1.
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

       01 WS-FRAUD-DATA.
          02 WS-FRD-ACCT-ID                PIC 9(11).
          02 WS-FRD-CUST-ID                PIC 9(9).
          02 WS-FRAUD-AUTH-RECORD          PIC X(200).
          02 WS-FRAUD-STATUS-RECORD.
             05 WS-FRD-ACTION              PIC X(01).
                88 WS-REPORT-FRAUD         VALUE 'F'.
                88 WS-REMOVE-FRAUD         VALUE 'R'.
             05 WS-FRD-UPDATE-STATUS       PIC X(01).
                88 WS-FRD-UPDT-SUCCESS     VALUE 'S'.
                88 WS-FRD-UPDT-FAILED      VALUE 'F'.
             05 WS-FRD-ACT-MSG             PIC X(50).




       COPY COCOM01Y.
          05 CDEMO-CPVD-INFO.
             10 CDEMO-CPVD-PAU-SEL-FLG     PIC X(01).
             10 CDEMO-CPVD-PAU-SELECTED    PIC X(08).
             10 CDEMO-CPVD-PAUKEY-PREV-PG  PIC X(08) OCCURS 20 TIMES.
             10 CDEMO-CPVD-PAUKEY-LAST     PIC X(08).
             10 CDEMO-CPVD-PAGE-NUM        PIC S9(04) COMP.
             10 CDEMO-CPVD-NEXT-PAGE-FLG   PIC X(01) VALUE 'N'.
                88 NEXT-PAGE-YES                     VALUE 'Y'.
                88 NEXT-PAGE-NO                      VALUE 'N'.
             10 CDEMO-CPVD-AUTH-KEYS       PIC X(08) OCCURS 5 TIMES.
             10 CDEMO-CPVD-FRAUD-DATA      PIC X(100).

       COPY COPAU01.


      *Screen Titles
       COPY COTTL01Y.

      *Current Date
       COPY CSDAT01Y.

      *Common Messages
       COPY CSMSG01Y.

      *Abend Variables
       COPY CSMSG02Y.

      *----------------------------------------------------------------*
      *  IMS SEGMENT LAYOUT
      *----------------------------------------------------------------*
      *- PENDING AUTHORIZATION SUMMARY SEGMENT - ROOT
       01 PENDING-AUTH-SUMMARY.
       COPY CIPAUSMY.

      *- PENDING AUTHORIZATION DETAILS SEGMENT - CHILD
       01 PENDING-AUTH-DETAILS.
       COPY CIPAUDTY.

       COPY DFHAID.
       COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
         05  LK-COMMAREA                           PIC X(01)
             OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.

       PROCEDURE DIVISION.
       MAIN-PARA.

           SET ERR-FLG-OFF     TO TRUE
           SET SEND-ERASE-YES  TO TRUE

           MOVE SPACES TO WS-MESSAGE
                          ERRMSGO OF COPAU1AO

           IF EIBCALEN = 0
               INITIALIZE CARDDEMO-COMMAREA

               MOVE WS-PGM-AUTH-SMRY        TO CDEMO-TO-PROGRAM
               PERFORM RETURN-TO-PREV-SCREEN
           ELSE
               MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA
               MOVE SPACES                  TO CDEMO-CPVD-FRAUD-DATA
               IF NOT CDEMO-PGM-REENTER
                   SET CDEMO-PGM-REENTER    TO TRUE
                   PERFORM PROCESS-ENTER-KEY

                   PERFORM SEND-AUTHVIEW-SCREEN
               ELSE
                   PERFORM RECEIVE-AUTHVIEW-SCREEN
                   EVALUATE EIBAID
                       WHEN DFHENTER
                           PERFORM PROCESS-ENTER-KEY
                           PERFORM SEND-AUTHVIEW-SCREEN
                       WHEN DFHPF3
                           MOVE WS-PGM-AUTH-SMRY     TO CDEMO-TO-PROGRAM
                           PERFORM RETURN-TO-PREV-SCREEN
                       WHEN DFHPF5
                           PERFORM MARK-AUTH-FRAUD
                           PERFORM SEND-AUTHVIEW-SCREEN
                       WHEN DFHPF8
                           PERFORM PROCESS-PF8-KEY
                           PERFORM SEND-AUTHVIEW-SCREEN
                       WHEN OTHER
                           PERFORM PROCESS-ENTER-KEY

                           MOVE CCDA-MSG-INVALID-KEY TO WS-MESSAGE
                           PERFORM SEND-AUTHVIEW-SCREEN
                   END-EVALUATE
               END-IF
           END-IF

           EXEC CICS RETURN
                     TRANSID (WS-CICS-TRANID)
                     COMMAREA (CARDDEMO-COMMAREA)
           END-EXEC
           .

       PROCESS-ENTER-KEY.

           MOVE LOW-VALUES          TO COPAU1AO
           IF CDEMO-ACCT-ID IS NUMERIC AND
              CDEMO-CPVD-PAU-SELECTED NOT = SPACES AND LOW-VALUES
              MOVE CDEMO-ACCT-ID            TO WS-ACCT-ID
              MOVE CDEMO-CPVD-PAU-SELECTED
                                            TO WS-AUTH-KEY
              PERFORM READ-AUTH-RECORD

              IF IMS-PSB-SCHD
                 SET IMS-PSB-NOT-SCHD      TO TRUE
                 PERFORM TAKE-SYNCPOINT
              END-IF

           ELSE
              SET ERR-FLG-ON                TO TRUE
           END-IF

           PERFORM POPULATE-AUTH-DETAILS
           .

       MARK-AUTH-FRAUD.
           MOVE CDEMO-ACCT-ID            TO WS-ACCT-ID
           MOVE CDEMO-CPVD-PAU-SELECTED  TO WS-AUTH-KEY

           PERFORM READ-AUTH-RECORD

           IF PA-FRAUD-CONFIRMED
              SET PA-FRAUD-REMOVED          TO TRUE
              SET WS-REMOVE-FRAUD           TO TRUE
           ELSE
              SET PA-FRAUD-CONFIRMED        TO TRUE
              SET WS-REPORT-FRAUD           TO TRUE
           END-IF

           MOVE PENDING-AUTH-DETAILS        TO WS-FRAUD-AUTH-RECORD
           MOVE CDEMO-ACCT-ID               TO WS-FRD-ACCT-ID
           MOVE CDEMO-CUST-ID               TO WS-FRD-CUST-ID

           EXEC CICS LINK
                PROGRAM(WS-PGM-AUTH-FRAUD)
                COMMAREA(WS-FRAUD-DATA)
                NOHANDLE
           END-EXEC
           IF EIBRESP = DFHRESP(NORMAL)
              IF WS-FRD-UPDT-SUCCESS
                 PERFORM UPDATE-AUTH-DETAILS
              ELSE
                 MOVE WS-FRD-ACT-MSG     TO WS-MESSAGE
                 PERFORM ROLL-BACK
              END-IF
           ELSE
              PERFORM ROLL-BACK
           END-IF

           MOVE PA-AUTHORIZATION-KEY     TO CDEMO-CPVD-PAU-SELECTED
           PERFORM POPULATE-AUTH-DETAILS
           .

       PROCESS-PF8-KEY.

           MOVE CDEMO-ACCT-ID            TO WS-ACCT-ID
           MOVE CDEMO-CPVD-PAU-SELECTED  TO WS-AUTH-KEY

           PERFORM READ-AUTH-RECORD
           PERFORM READ-NEXT-AUTH-RECORD

           IF IMS-PSB-SCHD
              SET IMS-PSB-NOT-SCHD      TO TRUE
              PERFORM TAKE-SYNCPOINT
           END-IF

           IF AUTHS-EOF
              SET SEND-ERASE-NO          TO TRUE
              MOVE 'Already at the last Authorization...'
                                         TO WS-MESSAGE
           ELSE
              MOVE PA-AUTHORIZATION-KEY  TO CDEMO-CPVD-PAU-SELECTED
              PERFORM POPULATE-AUTH-DETAILS
           END-IF
           .

       POPULATE-AUTH-DETAILS.


           IF ERR-FLG-OFF
               MOVE PA-CARD-NUM               TO CARDNUMO

               MOVE PA-AUTH-ORIG-DATE(1:2)    TO WS-CURDATE-YY
               MOVE PA-AUTH-ORIG-DATE(3:2)    TO WS-CURDATE-MM
               MOVE PA-AUTH-ORIG-DATE(5:2)    TO WS-CURDATE-DD
               MOVE WS-CURDATE-MM-DD-YY       TO WS-AUTH-DATE
               MOVE WS-AUTH-DATE              TO AUTHDTO

               MOVE PA-AUTH-ORIG-TIME(1:2)    TO WS-AUTH-TIME(1:2)
               MOVE PA-AUTH-ORIG-TIME(3:2)    TO WS-AUTH-TIME(4:2)
               MOVE PA-AUTH-ORIG-TIME(5:2)    TO WS-AUTH-TIME(7:2)
               MOVE WS-AUTH-TIME              TO AUTHTMO

               MOVE PA-APPROVED-AMT           TO WS-AUTH-AMT
               MOVE WS-AUTH-AMT               TO AUTHAMTO

               IF PA-AUTH-RESP-CODE = '00'
                  MOVE 'A'                    TO AUTHRSPO
                  MOVE DFHGREEN               TO AUTHRSPC
               ELSE
                  MOVE 'D'                    TO AUTHRSPO
                  MOVE DFHRED                 TO AUTHRSPC
               END-IF

               SEARCH ALL WS-DECLINE-REASON-TAB
                   AT END
                        MOVE '9999'                     TO AUTHRSNO
                        MOVE '-'                        TO AUTHRSNO(5:1)
                        MOVE 'ERROR'                    TO AUTHRSNO(6:)
                   WHEN DECL-CODE(WS-DECL-RSN-IDX) = PA-AUTH-RESP-REASON
                        MOVE PA-AUTH-RESP-REASON        TO AUTHRSNO
                        MOVE '-'                        TO AUTHRSNO(5:1)
                        MOVE DECL-DESC(WS-DECL-RSN-IDX) TO AUTHRSNO(6:)
               END-SEARCH


               MOVE PA-PROCESSING-CODE        TO AUTHCDO
               MOVE PA-POS-ENTRY-MODE         TO POSEMDO
               MOVE PA-MESSAGE-SOURCE         TO AUTHSRCO
               MOVE PA-MERCHANT-CATAGORY-CODE TO MCCCDO

               MOVE PA-CARD-EXPIRY-DATE(1:2)  TO CRDEXPO(1:2)
               MOVE '/'                       TO CRDEXPO(3:1)
               MOVE PA-CARD-EXPIRY-DATE(3:2)  TO CRDEXPO(4:2)

               MOVE PA-AUTH-TYPE              TO AUTHTYPO
               MOVE PA-TRANSACTION-ID         TO TRNIDO
               MOVE PA-MATCH-STATUS           TO AUTHMTCO

               IF PA-FRAUD-CONFIRMED OR PA-FRAUD-REMOVED
                  MOVE PA-AUTH-FRAUD          TO AUTHFRDO(1:1)
                  MOVE '-'                    TO AUTHFRDO(2:1)
                  MOVE PA-FRAUD-RPT-DATE      TO AUTHFRDO(3:)
               ELSE
                  MOVE '-'                    TO AUTHFRDO
               END-IF

               MOVE PA-MERCHANT-NAME          TO MERNAMEO
               MOVE PA-MERCHANT-ID            TO MERIDO
               MOVE PA-MERCHANT-CITY          TO MERCITYO
               MOVE PA-MERCHANT-STATE         TO MERSTO
               MOVE PA-MERCHANT-ZIP           TO MERZIPO
           END-IF
           .

       RETURN-TO-PREV-SCREEN.

           MOVE WS-CICS-TRANID TO CDEMO-FROM-TRANID
           MOVE WS-PGM-AUTH-DTL TO CDEMO-FROM-PROGRAM
           MOVE ZEROS          TO CDEMO-PGM-CONTEXT
           SET CDEMO-PGM-ENTER TO TRUE

           EXEC CICS
               XCTL PROGRAM(CDEMO-TO-PROGRAM)
               COMMAREA(CARDDEMO-COMMAREA)
           END-EXEC.


       SEND-AUTHVIEW-SCREEN.

           PERFORM POPULATE-HEADER-INFO

           MOVE WS-MESSAGE TO ERRMSGO OF COPAU1AO
           MOVE -1       TO CARDNUML

           IF SEND-ERASE-YES
              EXEC CICS SEND
                     MAP('COPAU1A')
                     MAPSET('COPAU01')
                     FROM(COPAU1AO)
                     ERASE
                     CURSOR
              END-EXEC
           ELSE
              EXEC CICS SEND
                     MAP('COPAU1A')
                     MAPSET('COPAU01')
                     FROM(COPAU1AO)
                     CURSOR
              END-EXEC
           END-IF
           .

       RECEIVE-AUTHVIEW-SCREEN.

           EXEC CICS RECEIVE
                     MAP('COPAU1A')
                     MAPSET('COPAU01')
                     INTO(COPAU1AI)
                     NOHANDLE
           END-EXEC
           .


       POPULATE-HEADER-INFO.

           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA

           MOVE CCDA-TITLE01           TO TITLE01O OF COPAU1AO
           MOVE CCDA-TITLE02           TO TITLE02O OF COPAU1AO
           MOVE WS-CICS-TRANID         TO TRNNAMEO OF COPAU1AO
           MOVE WS-PGM-AUTH-DTL        TO PGMNAMEO OF COPAU1AO

           MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
           MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
           MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY

           MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF COPAU1AO

           MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
           MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
           MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS

           MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF COPAU1AO
           .

       READ-AUTH-RECORD.

           PERFORM SCHEDULE-PSB


           MOVE WS-ACCT-ID                TO PA-ACCT-ID
           MOVE WS-AUTH-KEY               TO PA-AUTHORIZATION-KEY

           EXEC DLI GU USING PCB(PAUT-PCB-NUM)
               SEGMENT (PAUTSUM0)
               INTO (PENDING-AUTH-SUMMARY)
               WHERE (ACCNTID = PA-ACCT-ID)
           END-EXEC

           MOVE DIBSTAT                          TO IMS-RETURN-CODE
           EVALUATE TRUE
               WHEN STATUS-OK
                  SET AUTHS-NOT-EOF              TO TRUE
               WHEN SEGMENT-NOT-FOUND
               WHEN END-OF-DB
                  SET AUTHS-EOF                  TO TRUE
               WHEN OTHER
                  MOVE 'Y'     TO WS-ERR-FLG

                  STRING
                  ' System error while reading Auth Summary: Code:'
                  IMS-RETURN-CODE
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  PERFORM SEND-AUTHVIEW-SCREEN
           END-EVALUATE

           IF AUTHS-NOT-EOF
              EXEC DLI GNP USING PCB(PAUT-PCB-NUM)
                  SEGMENT (PAUTDTL1)
                  INTO (PENDING-AUTH-DETAILS)
                  WHERE (PAUT9CTS = PA-AUTHORIZATION-KEY)
              END-EXEC

              MOVE DIBSTAT                          TO IMS-RETURN-CODE
              EVALUATE TRUE
                  WHEN STATUS-OK
                     SET AUTHS-NOT-EOF              TO TRUE
                  WHEN SEGMENT-NOT-FOUND
                  WHEN END-OF-DB
                     SET AUTHS-EOF                  TO TRUE
                  WHEN OTHER
                     MOVE 'Y'     TO WS-ERR-FLG

                     STRING
                     ' System error while reading Auth Details: Code:'
                     IMS-RETURN-CODE
                     DELIMITED BY SIZE
                     INTO WS-MESSAGE
                     END-STRING
                     PERFORM SEND-AUTHVIEW-SCREEN
              END-EVALUATE
           END-IF

           .

       READ-NEXT-AUTH-RECORD.

           EXEC DLI GNP USING PCB(PAUT-PCB-NUM)
               SEGMENT (PAUTDTL1)
               INTO (PENDING-AUTH-DETAILS)
           END-EXEC

           MOVE DIBSTAT                          TO IMS-RETURN-CODE
           EVALUATE TRUE
               WHEN STATUS-OK
                  SET AUTHS-NOT-EOF              TO TRUE
               WHEN SEGMENT-NOT-FOUND
               WHEN END-OF-DB
                  SET AUTHS-EOF                  TO TRUE
               WHEN OTHER
                  MOVE 'Y'     TO WS-ERR-FLG

                  STRING
                  ' System error while reading next Auth: Code:'
                  IMS-RETURN-CODE
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  PERFORM SEND-AUTHVIEW-SCREEN
           END-EVALUATE
           .

       UPDATE-AUTH-DETAILS.

           MOVE WS-FRAUD-AUTH-RECORD           TO PENDING-AUTH-DETAILS
           DISPLAY 'RPT DT: ' PA-FRAUD-RPT-DATE

           EXEC DLI REPL USING PCB(PAUT-PCB-NUM)
                SEGMENT (PAUTDTL1)
                FROM (PENDING-AUTH-DETAILS)
           END-EXEC

           MOVE DIBSTAT                        TO IMS-RETURN-CODE
           EVALUATE TRUE
               WHEN STATUS-OK
                  PERFORM TAKE-SYNCPOINT
                  IF PA-FRAUD-REMOVED
                     MOVE 'AUTH FRAUD REMOVED...'   TO WS-MESSAGE
                  ELSE
                     MOVE 'AUTH MARKED FRAUD...'    TO WS-MESSAGE
                  END-IF
               WHEN OTHER
                  PERFORM ROLL-BACK

                  MOVE 'Y'     TO WS-ERR-FLG

                  STRING
                  ' System error while FRAUD Tagging, ROLLBACK||'
                  IMS-RETURN-CODE
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  PERFORM SEND-AUTHVIEW-SCREEN
           END-EVALUATE
           .

      *****************************************************************
      * TAKE SYNCPOINT                                                *
      *****************************************************************
       TAKE-SYNCPOINT.
           EXEC CICS SYNCPOINT
           END-EXEC
           .

      *****************************************************************
      * ROLLBACK THE DB CHANGES                                       *
      *****************************************************************
       ROLL-BACK.
           EXEC CICS
              SYNCPOINT ROLLBACK
           END-EXEC
           .

      *****************************************************************
      * SCHEDULE PSB                                                  *
      *****************************************************************
       SCHEDULE-PSB.
           EXEC DLI SCHD
                PSB((PSB-NAME))
                NODHABEND
           END-EXEC
           MOVE DIBSTAT        TO IMS-RETURN-CODE
           IF PSB-SCHEDULED-MORE-THAN-ONCE
              EXEC DLI TERM
              END-EXEC

              EXEC DLI SCHD
                   PSB((PSB-NAME))
                   NODHABEND
              END-EXEC
              MOVE DIBSTAT     TO IMS-RETURN-CODE
           END-IF
           IF STATUS-OK
              SET IMS-PSB-SCHD           TO TRUE
           ELSE
              MOVE 'Y'     TO WS-ERR-FLG

              STRING
                  ' System error while scheduling PSB: Code:'
              IMS-RETURN-CODE
              DELIMITED BY SIZE
              INTO WS-MESSAGE
              END-STRING
              PERFORM SEND-AUTHVIEW-SCREEN
           END-IF
           .

