      ******************************************************************
      * Program     : COPAUS0C.CBL
      * Application : CardDemo - Authorization Module
      * Type        : CICS COBOL IMS BMS Program
      * Function    : Summary View of Authoriation Messages
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
       PROGRAM-ID. COPAUS0C.                                                    
       AUTHOR.     AWS.                                                         
                                                                                
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
                                                                                
       DATA DIVISION.                                                           
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01 WS-VARIABLES.                                                         
         05 WS-PGM-AUTH-SMRY           PIC X(08) VALUE 'COPAUS0C'.              
         05 WS-PGM-AUTH-DTL            PIC X(08) VALUE 'COPAUS1C'.              
         05 WS-PGM-MENU                PIC X(08) VALUE 'COMEN01C'.              
         05 WS-CICS-TRANID             PIC X(04) VALUE 'CPVS'.                  
         05 WS-MESSAGE                 PIC X(80) VALUE SPACES.                  
         05 WS-ACCTFILENAME            PIC X(8)  VALUE 'ACCTDAT '.              
         05 WS-CUSTFILENAME            PIC X(8)  VALUE 'CUSTDAT '.              
         05 WS-CARDFILENAME            PIC X(8)  VALUE 'CARDDAT '.              
         05 WS-CARDXREFNAME-ACCT-PATH  PIC X(8)  VALUE 'CXACAIX '.              
         05 WS-CCXREF-FILE             PIC X(08) VALUE 'CCXREF  '.              
                                                                                
         05 WS-ACCT-ID                 PIC  X(11).                              
         05 WS-AUTH-KEY-SAVE           PIC  X(08).                              
         05 WS-AUTH-APRV-STAT          PIC  X(01).                              
         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.             
         05 WS-REAS-CD                 PIC S9(09) COMP VALUE ZEROS.             
         05 WS-RESP-CD-DIS             PIC  9(09).                              
         05 WS-REAS-CD-DIS             PIC  9(09).                              
         05 WS-REC-COUNT               PIC S9(04) COMP VALUE ZEROS.             
         05 WS-IDX                     PIC S9(04) COMP VALUE ZEROS.             
         05 WS-PAGE-NUM                PIC S9(04) COMP VALUE ZEROS.             
                                                                                
         05 WS-AUTH-AMT                PIC -zzzzzzz9.99.                        
         05 WS-DISPLAY-AMT12           PIC -zzzzzzz9.99.                        
         05 WS-DISPLAY-AMT9            PIC -zzzz9.99.                           
         05 WS-DISPLAY-COUNT           PIC 9(03).                               
         05 WS-AUTH-DATE               PIC X(08) VALUE '00/00/00'.              
         05 WS-AUTH-TIME               PIC X(08) VALUE '00:00:00'.              
                                                                                
      ******************************************************************        
      *      File and data Handling                                             
      ******************************************************************        
         05 WS-XREF-RID.                                                        
           10  WS-CARD-RID-CARDNUM                 PIC X(16).                   
           10  WS-CARD-RID-CUST-ID                 PIC 9(09).                   
           10  WS-CARD-RID-CUST-ID-X REDEFINES                                  
                  WS-CARD-RID-CUST-ID              PIC X(09).                   
           10  WS-CARD-RID-ACCT-ID                 PIC 9(11).                   
           10  WS-CARD-RID-ACCT-ID-X REDEFINES                                  
                  WS-CARD-RID-ACCT-ID              PIC X(11).                   
                                                                                
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
                                                                                
                                                                                
       01  WS-SWITCHES.                                                         
           05 WS-XREF-READ-FLG           PIC X(1).                              
              88 ACCT-NFOUND-XREF                  VALUE 'N'.                   
              88 ACCT-FOUND-XREF                   VALUE 'Y'.                   
           05 WS-ACCT-MASTER-READ-FLG    PIC X(1).                              
              88 FOUND-ACCT-IN-MSTR                VALUE 'Y'.                   
              88 NFOUND-ACCT-IN-MSTR               VALUE 'N'.                   
           05 WS-CUST-MASTER-READ-FLG    PIC X(1).                              
              88 FOUND-CUST-IN-MSTR                VALUE 'Y'.                   
              88 NFOUND-CUST-IN-MSTR               VALUE 'N'.                   
           05 WS-PAUT-SMRY-SEG-FLG       PIC X(1).                              
              88 FOUND-PAUT-SMRY-SEG               VALUE 'Y'.                   
              88 NFOUND-PAUT-SMRY-SEG              VALUE 'N'.                   
           05 WS-ERR-FLG                 PIC X(1)  VALUE 'N'.                   
              88 ERR-FLG-ON                        VALUE 'Y'.                   
              88 ERR-FLG-OFF                       VALUE 'N'.                   
           05 WS-AUTHS-EOF               PIC X(1)  VALUE 'N'.                   
              88 AUTHS-EOF                         VALUE 'Y'.                   
              88 AUTHS-NOT-EOF                     VALUE 'N'.                   
           05 WS-SEND-ERASE-FLG          PIC X(1)  VALUE 'Y'.                   
              88 SEND-ERASE-YES                    VALUE 'Y'.                   
              88 SEND-ERASE-NO                     VALUE 'N'.                   
                                                                                
       COPY COCOM01Y.                                                           
          05 CDEMO-CPVS-INFO.                                                   
             10 CDEMO-CPVS-PAU-SEL-FLG     PIC X(01).                           
             10 CDEMO-CPVS-PAU-SELECTED    PIC X(08).                           
             10 CDEMO-CPVS-PAUKEY-PREV-PG  PIC X(08) OCCURS 20 TIMES.           
             10 CDEMO-CPVS-PAUKEY-LAST     PIC X(08).                           
             10 CDEMO-CPVS-PAGE-NUM        PIC S9(04) COMP.                     
             10 CDEMO-CPVS-NEXT-PAGE-FLG   PIC X(01) VALUE 'N'.                 
                88 NEXT-PAGE-YES                     VALUE 'Y'.                 
                88 NEXT-PAGE-NO                      VALUE 'N'.                 
             10 CDEMO-CPVS-AUTH-KEYS       PIC X(08) OCCURS 5 TIMES.            
                                                                                
      *BMS Copybook
       COPY COPAU00.

      *Screen Titles
       COPY COTTL01Y.

      *Current Date
       COPY CSDAT01Y.

      *Common Messages
       COPY CSMSG01Y.

      *Abend Variables
       COPY CSMSG02Y.

      *ACCOUNT RECORD LAYOUT
       COPY CVACT01Y.

      *CUSTOMER RECORD LAYOUT
       COPY CVACT02Y.

      *CARD XREF LAYOUT
       COPY CVACT03Y.

      *CUSTOMER LAYOUT
       COPY CVCUS01Y.

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
      *****************************************************************
       MAIN-PARA.
      *****************************************************************

           SET ERR-FLG-OFF TO TRUE
           SET AUTHS-NOT-EOF TO TRUE
           SET NEXT-PAGE-NO TO TRUE
           SET SEND-ERASE-YES TO TRUE

           MOVE SPACES TO WS-MESSAGE ERRMSGO OF COPAU0AO

           MOVE -1       TO ACCTIDL OF COPAU0AI

           IF EIBCALEN = 0
               INITIALIZE CARDDEMO-COMMAREA
               MOVE WS-PGM-AUTH-SMRY    TO CDEMO-TO-PROGRAM

               SET CDEMO-PGM-REENTER    TO TRUE
               MOVE LOW-VALUES          TO COPAU0AO
               MOVE -1                  TO ACCTIDL OF COPAU0AI

              PERFORM SEND-PAULST-SCREEN
           ELSE
               MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA

               IF NOT CDEMO-PGM-REENTER
                  SET CDEMO-PGM-REENTER     TO TRUE

                  MOVE LOW-VALUES           TO COPAU0AO

                  IF CDEMO-ACCT-ID IS NUMERIC
                     MOVE CDEMO-ACCT-ID     TO WS-ACCT-ID
                                               ACCTIDO OF COPAU0AO
                  ELSE
                     MOVE SPACE             TO ACCTIDO OF COPAU0AO
                     MOVE LOW-VALUES        TO WS-ACCT-ID
                  END-IF

                  PERFORM GATHER-DETAILS

                  SET SEND-ERASE-YES TO TRUE

                  PERFORM SEND-PAULST-SCREEN

               ELSE
                  PERFORM RECEIVE-PAULST-SCREEN

                  EVALUATE EIBAID
                     WHEN DFHENTER
                       PERFORM PROCESS-ENTER-KEY

                       IF WS-ACCT-ID = LOW-VALUES
                          MOVE SPACE           TO ACCTIDO   OF COPAU0AO
                       ELSE
                          MOVE WS-ACCT-ID      TO ACCTIDO   OF COPAU0AO
                       END-IF

                       PERFORM SEND-PAULST-SCREEN
                     WHEN DFHPF3
                       MOVE WS-PGM-MENU        TO CDEMO-TO-PROGRAM
                       PERFORM RETURN-TO-PREV-SCREEN
                       PERFORM SEND-PAULST-SCREEN
                     WHEN DFHPF7
                       PERFORM PROCESS-PF7-KEY
                       PERFORM SEND-PAULST-SCREEN
                     WHEN DFHPF8
                       PERFORM PROCESS-PF8-KEY
                       PERFORM SEND-PAULST-SCREEN
                     WHEN OTHER
                       MOVE 'Y'              TO WS-ERR-FLG
                       MOVE -1               TO ACCTIDL OF COPAU0AI
                       MOVE CCDA-MSG-INVALID-KEY  TO WS-MESSAGE
                       PERFORM SEND-PAULST-SCREEN
                  END-EVALUATE
               END-IF
           END-IF

           EXEC CICS RETURN
                     TRANSID (WS-CICS-TRANID)
                     COMMAREA (CARDDEMO-COMMAREA)
           END-EXEC.


      *****************************************************************
       PROCESS-ENTER-KEY.
      *****************************************************************

           IF ACCTIDI OF COPAU0AI = SPACES OR LOW-VALUES
              MOVE LOW-VALUES                 TO WS-ACCT-ID

              MOVE 'Y'                        TO WS-ERR-FLG
              MOVE
              'Please enter Acct Id...'       TO WS-MESSAGE

              MOVE -1                         TO ACCTIDL OF COPAU0AI
           ELSE
              IF ACCTIDI OF COPAU0AI IS NOT NUMERIC
                MOVE LOW-VALUES               TO WS-ACCT-ID

                MOVE 'Y'                      TO WS-ERR-FLG
                MOVE
                'Acct Id must be Numeric ...' TO WS-MESSAGE

                MOVE -1                       TO ACCTIDL OF COPAU0AI

              ELSE
                MOVE ACCTIDI OF COPAU0AI      TO WS-ACCT-ID
                                                 CDEMO-ACCT-ID
                EVALUATE TRUE
                  WHEN SEL0001I OF COPAU0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0001I OF COPAU0AI TO CDEMO-CPVS-PAU-SEL-FLG
                   MOVE CDEMO-CPVS-AUTH-KEYS(1)
                                             TO CDEMO-CPVS-PAU-SELECTED
                  WHEN SEL0002I OF COPAU0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0002I OF COPAU0AI TO CDEMO-CPVS-PAU-SEL-FLG
                   MOVE CDEMO-CPVS-AUTH-KEYS(2)
                                             TO CDEMO-CPVS-PAU-SELECTED
                  WHEN SEL0003I OF COPAU0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0003I OF COPAU0AI TO CDEMO-CPVS-PAU-SEL-FLG
                   MOVE CDEMO-CPVS-AUTH-KEYS(3)
                                             TO CDEMO-CPVS-PAU-SELECTED
                  WHEN SEL0004I OF COPAU0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0004I OF COPAU0AI TO CDEMO-CPVS-PAU-SEL-FLG
                   MOVE CDEMO-CPVS-AUTH-KEYS(4)
                                             TO CDEMO-CPVS-PAU-SELECTED
                  WHEN SEL0005I OF COPAU0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0005I OF COPAU0AI TO CDEMO-CPVS-PAU-SEL-FLG
                   MOVE CDEMO-CPVS-AUTH-KEYS(5)
                                             TO CDEMO-CPVS-PAU-SELECTED
                  WHEN OTHER
                   MOVE SPACES   TO CDEMO-CPVS-PAU-SEL-FLG
                   MOVE SPACES   TO CDEMO-CPVS-PAU-SELECTED
                END-EVALUATE
                IF (CDEMO-CPVS-PAU-SEL-FLG NOT = SPACES AND LOW-VALUES)
                   AND
                   (CDEMO-CPVS-PAU-SELECTED NOT = SPACES AND LOW-VALUES)
                   EVALUATE CDEMO-CPVS-PAU-SEL-FLG
                     WHEN 'S'
                     WHEN 's'
                        MOVE WS-PGM-AUTH-DTL  TO CDEMO-TO-PROGRAM
                        MOVE WS-CICS-TRANID   TO CDEMO-FROM-TRANID
                        MOVE WS-PGM-AUTH-SMRY TO CDEMO-FROM-PROGRAM
                        MOVE 0                TO CDEMO-PGM-CONTEXT
                        SET CDEMO-PGM-ENTER   TO TRUE

                        EXEC CICS
                            XCTL PROGRAM(CDEMO-TO-PROGRAM)
                            COMMAREA(CARDDEMO-COMMAREA)
                        END-EXEC
                     WHEN OTHER
                       MOVE
                       'Invalid selection. Valid value is S'
                                              TO WS-MESSAGE
                       MOVE -1                TO ACCTIDL OF COPAU0AI
                   END-EVALUATE
                END-IF

              END-IF
           END-IF

           PERFORM GATHER-DETAILS
           .


      *****************************************************************
       GATHER-DETAILS.
      *****************************************************************

           MOVE -1       TO ACCTIDL OF COPAU0AI

           MOVE 0        TO CDEMO-CPVS-PAGE-NUM

           IF WS-ACCT-ID NOT = LOW-VALUES
              PERFORM GATHER-ACCOUNT-DETAILS

              PERFORM INITIALIZE-AUTH-DATA

              IF FOUND-PAUT-SMRY-SEG
                 PERFORM PROCESS-PAGE-FORWARD
              END-IF
           END-IF
           .


      *****************************************************************
       PROCESS-PF7-KEY.
      *****************************************************************

           IF CDEMO-CPVS-PAGE-NUM > 1
              COMPUTE CDEMO-CPVS-PAGE-NUM = CDEMO-CPVS-PAGE-NUM - 1

              MOVE CDEMO-CPVS-PAUKEY-PREV-PG(CDEMO-CPVS-PAGE-NUM)
                                           TO WS-AUTH-KEY-SAVE
              PERFORM GET-AUTH-SUMMARY

              SET SEND-ERASE-NO            TO TRUE

              SET NEXT-PAGE-YES            TO TRUE
              MOVE -1                      TO ACCTIDL OF COPAU0AI

              PERFORM INITIALIZE-AUTH-DATA

              PERFORM PROCESS-PAGE-FORWARD
           ELSE
              MOVE 'You are already at the top of the page...' TO
                               WS-MESSAGE
              SET SEND-ERASE-NO            TO TRUE
           END-IF
           .

      *****************************************************************
       PROCESS-PF8-KEY.
      *****************************************************************

           IF CDEMO-CPVS-PAUKEY-LAST = SPACES OR LOW-VALUES
               MOVE LOW-VALUES             TO WS-AUTH-KEY-SAVE
           ELSE
               MOVE CDEMO-CPVS-PAUKEY-LAST TO WS-AUTH-KEY-SAVE

               PERFORM GET-AUTH-SUMMARY
               PERFORM REPOSITION-AUTHORIZATIONS
           END-IF

           MOVE -1                         TO ACCTIDL OF COPAU0AI

           SET SEND-ERASE-NO               TO TRUE

           IF NEXT-PAGE-YES
               PERFORM INITIALIZE-AUTH-DATA

               PERFORM PROCESS-PAGE-FORWARD
           ELSE
               MOVE 'You are already at the bottom of the page...'
                                           TO WS-MESSAGE
           END-IF
           .

      *****************************************************************
       PROCESS-PAGE-FORWARD.
      *****************************************************************

           IF ERR-FLG-OFF

               MOVE 1             TO  WS-IDX

               MOVE LOW-VALUES    TO CDEMO-CPVS-PAUKEY-LAST

               PERFORM UNTIL WS-IDX > 5 OR AUTHS-EOF OR ERR-FLG-ON
                   IF EIBAID = DFHPF7 AND WS-IDX = 1
                      PERFORM REPOSITION-AUTHORIZATIONS
                   ELSE
                      PERFORM GET-AUTHORIZATIONS
                   END-IF
                   IF AUTHS-NOT-EOF AND ERR-FLG-OFF
                       PERFORM POPULATE-AUTH-LIST
                       COMPUTE WS-IDX = WS-IDX + 1

                       MOVE PA-AUTHORIZATION-KEY TO
                                             CDEMO-CPVS-PAUKEY-LAST
                       IF WS-IDX = 2
                          COMPUTE CDEMO-CPVS-PAGE-NUM =
                                  CDEMO-CPVS-PAGE-NUM + 1
                          MOVE PA-AUTHORIZATION-KEY TO
                          CDEMO-CPVS-PAUKEY-PREV-PG(CDEMO-CPVS-PAGE-NUM)
                       END-IF
                   END-IF
               END-PERFORM

               IF AUTHS-NOT-EOF AND ERR-FLG-OFF
                   PERFORM GET-AUTHORIZATIONS
                   IF AUTHS-NOT-EOF AND ERR-FLG-OFF
                       SET NEXT-PAGE-YES TO TRUE
                   ELSE
                       SET NEXT-PAGE-NO TO TRUE
                   END-IF
               END-IF

           END-IF.


      *****************************************************************
       GET-AUTHORIZATIONS.
      *****************************************************************

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
                  ' System error while reading AUTH Details: Code:'
                  IMS-RETURN-CODE
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  MOVE -1       TO ACCTIDL OF COPAU0AI
                  PERFORM SEND-PAULST-SCREEN
           END-EVALUATE

           .
      *****************************************************************
       REPOSITION-AUTHORIZATIONS.
      *****************************************************************

           MOVE WS-AUTH-KEY-SAVE          TO PA-AUTHORIZATION-KEY

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
                  ' System error while repos. AUTH Details: Code:'
                  IMS-RETURN-CODE
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  MOVE -1       TO ACCTIDL OF COPAU0AI
                  PERFORM SEND-PAULST-SCREEN
           END-EVALUATE

           .

      *****************************************************************
       POPULATE-AUTH-LIST.
      *****************************************************************

           MOVE PA-APPROVED-AMT           TO WS-AUTH-AMT

           MOVE PA-AUTH-ORIG-TIME(1:2)    TO WS-AUTH-TIME(1:2)
           MOVE PA-AUTH-ORIG-TIME(3:2)    TO WS-AUTH-TIME(4:2)
           MOVE PA-AUTH-ORIG-TIME(5:2)    TO WS-AUTH-TIME(7:2)

           MOVE PA-AUTH-ORIG-DATE(1:2)    TO WS-CURDATE-YY
           MOVE PA-AUTH-ORIG-DATE(3:2)    TO WS-CURDATE-MM
           MOVE PA-AUTH-ORIG-DATE(5:2)    TO WS-CURDATE-DD
           MOVE WS-CURDATE-MM-DD-YY       TO WS-AUTH-DATE

           IF PA-AUTH-RESP-CODE = '00'
              MOVE 'A'               TO WS-AUTH-APRV-STAT
           ELSE
              MOVE 'D'               TO WS-AUTH-APRV-STAT
           END-IF

           EVALUATE WS-IDX
               WHEN 1
                   MOVE PA-AUTHORIZATION-KEY
                                          TO CDEMO-CPVS-AUTH-KEYS(1)

                   MOVE PA-TRANSACTION-ID TO TRNID01I OF COPAU0AI
                   MOVE WS-AUTH-DATE      TO PDATE01I OF COPAU0AI
                   MOVE WS-AUTH-TIME      TO PTIME01I OF COPAU0AI
                   MOVE PA-AUTH-TYPE      TO PTYPE01I OF COPAU0AI
                   MOVE WS-AUTH-APRV-STAT TO PAPRV01I OF COPAU0AI
                   MOVE PA-MATCH-STATUS   TO PSTAT01I OF COPAU0AI
                   MOVE WS-AUTH-AMT       TO PAMT001I OF COPAU0AI
                   MOVE DFHBMUNP          TO SEL0001A OF COPAU0AI
               WHEN 2
                   MOVE PA-AUTHORIZATION-KEY
                                          TO CDEMO-CPVS-AUTH-KEYS(2)

                   MOVE PA-TRANSACTION-ID TO TRNID02I OF COPAU0AI
                   MOVE WS-AUTH-DATE      TO PDATE02I OF COPAU0AI
                   MOVE WS-AUTH-TIME      TO PTIME02I OF COPAU0AI
                   MOVE PA-AUTH-TYPE      TO PTYPE02I OF COPAU0AI
                   MOVE WS-AUTH-APRV-STAT TO PAPRV02I OF COPAU0AI
                   MOVE PA-MATCH-STATUS   TO PSTAT02I OF COPAU0AI
                   MOVE WS-AUTH-AMT       TO PAMT002I OF COPAU0AI
                   MOVE DFHBMUNP          TO SEL0002A OF COPAU0AI
               WHEN 3
                   MOVE PA-AUTHORIZATION-KEY
                                          TO CDEMO-CPVS-AUTH-KEYS(3)

                   MOVE PA-TRANSACTION-ID TO TRNID03I OF COPAU0AI
                   MOVE WS-AUTH-DATE      TO PDATE03I OF COPAU0AI
                   MOVE WS-AUTH-TIME      TO PTIME03I OF COPAU0AI
                   MOVE PA-AUTH-TYPE      TO PTYPE03I OF COPAU0AI
                   MOVE WS-AUTH-APRV-STAT TO PAPRV03I OF COPAU0AI
                   MOVE PA-MATCH-STATUS   TO PSTAT03I OF COPAU0AI
                   MOVE WS-AUTH-AMT       TO PAMT003I OF COPAU0AI
                   MOVE DFHBMUNP          TO SEL0003A OF COPAU0AI
               WHEN 4
                   MOVE PA-AUTHORIZATION-KEY
                                          TO CDEMO-CPVS-AUTH-KEYS(4)

                   MOVE PA-TRANSACTION-ID TO TRNID04I OF COPAU0AI
                   MOVE WS-AUTH-DATE      TO PDATE04I OF COPAU0AI
                   MOVE WS-AUTH-TIME      TO PTIME04I OF COPAU0AI
                   MOVE PA-AUTH-TYPE      TO PTYPE04I OF COPAU0AI
                   MOVE WS-AUTH-APRV-STAT TO PAPRV04I OF COPAU0AI
                   MOVE PA-MATCH-STATUS   TO PSTAT04I OF COPAU0AI
                   MOVE WS-AUTH-AMT       TO PAMT004I OF COPAU0AI
                   MOVE DFHBMUNP          TO SEL0004A OF COPAU0AI
               WHEN 5
                   MOVE PA-AUTHORIZATION-KEY
                                          TO CDEMO-CPVS-AUTH-KEYS(5)

                   MOVE PA-TRANSACTION-ID TO TRNID05I OF COPAU0AI
                   MOVE WS-AUTH-DATE      TO PDATE05I OF COPAU0AI
                   MOVE WS-AUTH-TIME      TO PTIME05I OF COPAU0AI
                   MOVE PA-AUTH-TYPE      TO PTYPE05I OF COPAU0AI
                   MOVE WS-AUTH-APRV-STAT TO PAPRV05I OF COPAU0AI
                   MOVE PA-MATCH-STATUS   TO PSTAT05I OF COPAU0AI
                   MOVE WS-AUTH-AMT       TO PAMT005I OF COPAU0AI
                   MOVE DFHBMUNP          TO SEL0005A OF COPAU0AI
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

      *****************************************************************
       INITIALIZE-AUTH-DATA.
      *****************************************************************

           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 5
             EVALUATE WS-IDX
               WHEN 1
                   MOVE DFHBMPRO TO SEL0001A OF COPAU0AI
                   MOVE SPACES   TO TRNID01I OF COPAU0AI
                   MOVE SPACES   TO PDATE01I OF COPAU0AI
                   MOVE SPACES   TO PTIME01I OF COPAU0AI
                   MOVE SPACES   TO PTYPE01I OF COPAU0AI
                   MOVE SPACES   TO PAPRV01I OF COPAU0AI
                   MOVE SPACES   TO PSTAT01I OF COPAU0AI
                   MOVE SPACES   TO PAMT001I OF COPAU0AI
               WHEN 2
                   MOVE DFHBMPRO TO SEL0002A OF COPAU0AI
                   MOVE SPACES   TO TRNID02I OF COPAU0AI
                   MOVE SPACES   TO PDATE02I OF COPAU0AI
                   MOVE SPACES   TO PTIME02I OF COPAU0AI
                   MOVE SPACES   TO PTYPE02I OF COPAU0AI
                   MOVE SPACES   TO PAPRV02I OF COPAU0AI
                   MOVE SPACES   TO PSTAT02I OF COPAU0AI
                   MOVE SPACES   TO PAMT002I OF COPAU0AI
               WHEN 3
                   MOVE DFHBMPRO TO SEL0003A OF COPAU0AI
                   MOVE SPACES   TO TRNID03I OF COPAU0AI
                   MOVE SPACES   TO PDATE03I OF COPAU0AI
                   MOVE SPACES   TO PTIME03I OF COPAU0AI
                   MOVE SPACES   TO PTYPE03I OF COPAU0AI
                   MOVE SPACES   TO PAPRV03I OF COPAU0AI
                   MOVE SPACES   TO PSTAT03I OF COPAU0AI
                   MOVE SPACES   TO PAMT003I OF COPAU0AI
               WHEN 4
                   MOVE DFHBMPRO TO SEL0004A OF COPAU0AI
                   MOVE SPACES   TO TRNID04I OF COPAU0AI
                   MOVE SPACES   TO PDATE04I OF COPAU0AI
                   MOVE SPACES   TO PTIME04I OF COPAU0AI
                   MOVE SPACES   TO PTYPE04I OF COPAU0AI
                   MOVE SPACES   TO PAPRV04I OF COPAU0AI
                   MOVE SPACES   TO PSTAT04I OF COPAU0AI
                   MOVE SPACES   TO PAMT004I OF COPAU0AI
               WHEN 5
                   MOVE DFHBMPRO TO SEL0005A OF COPAU0AI
                   MOVE SPACES   TO TRNID05I OF COPAU0AI
                   MOVE SPACES   TO PDATE05I OF COPAU0AI
                   MOVE SPACES   TO PTIME05I OF COPAU0AI
                   MOVE SPACES   TO PTYPE05I OF COPAU0AI
                   MOVE SPACES   TO PAPRV05I OF COPAU0AI
                   MOVE SPACES   TO PSTAT05I OF COPAU0AI
                   MOVE SPACES   TO PAMT005I OF COPAU0AI
               WHEN OTHER
                   CONTINUE
             END-EVALUATE
           END-PERFORM
           .

      *****************************************************************
       RETURN-TO-PREV-SCREEN.
      *****************************************************************

           IF CDEMO-TO-PROGRAM = LOW-VALUES OR SPACES
               MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM
           END-IF
           MOVE WS-CICS-TRANID  TO CDEMO-FROM-TRANID
           MOVE WS-PGM-AUTH-SMRY TO CDEMO-FROM-PROGRAM
           MOVE ZEROS           TO CDEMO-PGM-CONTEXT
           EXEC CICS
               XCTL PROGRAM(CDEMO-TO-PROGRAM)
               COMMAREA(CARDDEMO-COMMAREA)
           END-EXEC.


      *****************************************************************
       SEND-PAULST-SCREEN.
      *****************************************************************

           IF IMS-PSB-SCHD
              SET IMS-PSB-NOT-SCHD      TO TRUE
              EXEC CICS SYNCPOINT
              END-EXEC
           END-IF

           PERFORM POPULATE-HEADER-INFO

           MOVE WS-MESSAGE TO ERRMSGO OF COPAU0AO

           IF SEND-ERASE-YES
               EXEC CICS SEND
                         MAP('COPAU0A')
                         MAPSET('COPAU00')
                         FROM(COPAU0AO)
                         ERASE
                         CURSOR
               END-EXEC
           ELSE
               EXEC CICS SEND
                         MAP('COPAU0A')
                         MAPSET('COPAU00')
                         FROM(COPAU0AO)
                         CURSOR
               END-EXEC
           END-IF.

      *****************************************************************
       RECEIVE-PAULST-SCREEN.
      *****************************************************************

           EXEC CICS RECEIVE
                     MAP('COPAU0A')
                     MAPSET('COPAU00')
                     INTO(COPAU0AI)
                     RESP(WS-RESP-CD)
                     RESP2(WS-REAS-CD)
           END-EXEC
           .


      *****************************************************************
       POPULATE-HEADER-INFO.
      *****************************************************************

           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA

           MOVE CCDA-TITLE01           TO TITLE01O OF COPAU0AO
           MOVE CCDA-TITLE02           TO TITLE02O OF COPAU0AO
           MOVE WS-CICS-TRANID         TO TRNNAMEO OF COPAU0AO
           MOVE WS-PGM-AUTH-SMRY       TO PGMNAMEO OF COPAU0AO

           MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
           MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
           MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY

           MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF COPAU0AO

           MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
           MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
           MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS

           MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF COPAU0AO
           .

      *****************************************************************
       GATHER-ACCOUNT-DETAILS.
      *****************************************************************

           PERFORM GETCARDXREF-BYACCT
           PERFORM GETACCTDATA-BYACCT
           PERFORM GETCUSTDATA-BYCUST

           MOVE CUST-ID                TO CUSTIDO
           STRING CUST-FIRST-NAME DELIMITED BY SPACES
                  ' ' DELIMITED BY SIZE
                  CUST-MIDDLE-NAME(1:1) DELIMITED BY SIZE
                  ' ' DELIMITED BY SIZE
                  CUST-LAST-NAME DELIMITED BY SPACES
                  INTO CNAMEO
           END-STRING

           STRING CUST-ADDR-LINE-1 DELIMITED BY '  '
                  ',' DELIMITED BY SIZE
                  CUST-ADDR-LINE-2 DELIMITED BY '  '
                  INTO ADDR001O
           END-STRING
           STRING CUST-ADDR-LINE-3 DELIMITED BY '  '
                  ',' DELIMITED BY SIZE
                  CUST-ADDR-STATE-CD DELIMITED BY SIZE
                  ',' DELIMITED BY SIZE
                  CUST-ADDR-ZIP(1:5) DELIMITED BY SIZE
                  INTO ADDR002O
           END-STRING

           MOVE CUST-PHONE-NUM-1       TO PHONE1O
           MOVE ACCT-CREDIT-LIMIT      TO WS-DISPLAY-AMT12
           MOVE WS-DISPLAY-AMT12       TO CREDLIMO
           MOVE ACCT-CASH-CREDIT-LIMIT TO WS-DISPLAY-AMT9
           MOVE WS-DISPLAY-AMT9        TO CASHLIMO

           PERFORM GET-AUTH-SUMMARY

           IF FOUND-PAUT-SMRY-SEG
              MOVE PA-APPROVED-AUTH-CNT   TO WS-DISPLAY-COUNT
              MOVE WS-DISPLAY-COUNT       TO APPRCNTO
              MOVE PA-DECLINED-AUTH-CNT   TO WS-DISPLAY-COUNT
              MOVE WS-DISPLAY-COUNT       TO DECLCNTO
              MOVE PA-CREDIT-BALANCE      TO WS-DISPLAY-AMT12
              MOVE WS-DISPLAY-AMT12       TO CREDBALO
              MOVE PA-CASH-BALANCE        TO WS-DISPLAY-AMT9
              MOVE WS-DISPLAY-AMT9        TO CASHBALO
              MOVE PA-APPROVED-AUTH-AMT   TO WS-DISPLAY-AMT9
              MOVE WS-DISPLAY-AMT9        TO APPRAMTO
              MOVE PA-DECLINED-AUTH-AMT   TO WS-DISPLAY-AMT9
              MOVE WS-DISPLAY-AMT9        TO DECLAMTO
           ELSE
              MOVE ZERO                   TO APPRCNTO
                                             DECLCNTO
                                             CREDBALO
                                             CASHBALO
                                             APPRAMTO
                                             DECLAMTO
           END-IF
           .


      *****************************************************************
       GETCARDXREF-BYACCT.
      *****************************************************************

      *    Read the Card file. Access via alternate index ACCTID
      *
           MOVE WS-ACCT-ID          TO WS-CARD-RID-ACCT-ID-X
           EXEC CICS READ
                DATASET   (WS-CARDXREFNAME-ACCT-PATH)
                RIDFLD    (WS-CARD-RID-ACCT-ID-X)
                KEYLENGTH (LENGTH OF WS-CARD-RID-ACCT-ID-X)
                INTO      (CARD-XREF-RECORD)
                LENGTH    (LENGTH OF CARD-XREF-RECORD)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                  MOVE XREF-CUST-ID               TO CDEMO-CUST-ID
                  MOVE XREF-CARD-NUM              TO CDEMO-CARD-NUM
               WHEN DFHRESP(NOTFND)
                  MOVE WS-RESP-CD        TO WS-RESP-CD-DIS
                  MOVE WS-REAS-CD        TO WS-REAS-CD-DIS

                  STRING
                  'Account:'
                   WS-ACCT-ID
                  ' not found in XREF file. Resp:' WS-RESP-CD-DIS
                  ' Reas:' WS-REAS-CD-DIS
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  MOVE -1       TO ACCTIDL OF COPAU0AI
                  PERFORM SEND-PAULST-SCREEN
               WHEN OTHER
                  MOVE 'Y'     TO WS-ERR-FLG
                  MOVE WS-RESP-CD        TO WS-RESP-CD-DIS
                  MOVE WS-REAS-CD        TO WS-REAS-CD-DIS

                  STRING
                  'Account:'
                   WS-CARD-RID-ACCT-ID-X
                  ' System error while reading XREF file. Resp:'
                  WS-RESP-CD-DIS ' Reas:' WS-REAS-CD-DIS
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  MOVE -1       TO ACCTIDL OF COPAU0AI
                  PERFORM SEND-PAULST-SCREEN
           END-EVALUATE
           .

      *****************************************************************
       GETACCTDATA-BYACCT.
      *****************************************************************

           MOVE XREF-ACCT-ID            TO WS-CARD-RID-ACCT-ID
           EXEC CICS READ
                DATASET   (WS-ACCTFILENAME)
                RIDFLD    (WS-CARD-RID-ACCT-ID-X)
                KEYLENGTH (LENGTH OF WS-CARD-RID-ACCT-ID-X)
                INTO      (ACCOUNT-RECORD)
                LENGTH    (LENGTH OF ACCOUNT-RECORD)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                  continue
               WHEN DFHRESP(NOTFND)
                  MOVE WS-RESP-CD        TO WS-RESP-CD-DIS
                  MOVE WS-REAS-CD        TO WS-REAS-CD-DIS

                  STRING
                  'Account:'
                   WS-CARD-RID-ACCT-ID-X
                  ' not found in ACCT file. Resp:' WS-RESP-CD-DIS
                  ' Reas:' WS-REAS-CD-DIS
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  MOVE -1       TO ACCTIDL OF COPAU0AI
                  PERFORM SEND-PAULST-SCREEN
               WHEN OTHER
                  MOVE 'Y'     TO WS-ERR-FLG
                  MOVE WS-RESP-CD        TO WS-RESP-CD-DIS
                  MOVE WS-REAS-CD        TO WS-REAS-CD-DIS

                  STRING
                  'Account:'
                   WS-CARD-RID-ACCT-ID-X
                  ' System error while reading ACCT file. Resp:'
                  WS-RESP-CD-DIS ' Reas:' WS-REAS-CD-DIS
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  MOVE -1       TO ACCTIDL OF COPAU0AI
                  PERFORM SEND-PAULST-SCREEN
           END-EVALUATE
           .

      *****************************************************************
       GETCUSTDATA-BYCUST.
      *****************************************************************

           MOVE XREF-CUST-ID              TO WS-CARD-RID-CUST-ID

           EXEC CICS READ
                DATASET   (WS-CUSTFILENAME)
                RIDFLD    (WS-CARD-RID-CUST-ID-X)
                KEYLENGTH (LENGTH OF WS-CARD-RID-CUST-ID-X)
                INTO      (CUSTOMER-RECORD)
                LENGTH    (LENGTH OF CUSTOMER-RECORD)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                  CONTINUE
               WHEN DFHRESP(NOTFND)
                  MOVE WS-RESP-CD        TO WS-RESP-CD-DIS
                  MOVE WS-REAS-CD        TO WS-REAS-CD-DIS

                  STRING
                  'Customer:'
                   WS-CARD-RID-CUST-ID-X
                  ' not found in CUST file. Resp:' WS-RESP-CD-DIS
                  ' Reas:' WS-REAS-CD-DIS
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  MOVE -1       TO ACCTIDL OF COPAU0AI
                  PERFORM SEND-PAULST-SCREEN
               WHEN OTHER
                  MOVE 'Y'     TO WS-ERR-FLG
                  MOVE WS-RESP-CD        TO WS-RESP-CD-DIS
                  MOVE WS-REAS-CD        TO WS-REAS-CD-DIS

                  STRING
                  'Customer:'
                   WS-CARD-RID-CUST-ID-X
                  ' System error while reading CUST file. Resp:'
                  WS-RESP-CD-DIS ' Reas:' WS-REAS-CD-DIS
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  MOVE -1       TO ACCTIDL OF COPAU0AI
                  PERFORM SEND-PAULST-SCREEN
           END-EVALUATE
           .

      *****************************************************************
       GET-AUTH-SUMMARY.
      *****************************************************************

           PERFORM SCHEDULE-PSB

           MOVE CDEMO-ACCT-ID                   TO PA-ACCT-ID
      *    MOVE XREF-ACCT-ID                    TO PA-ACCT-ID
           EXEC DLI GU USING PCB(PAUT-PCB-NUM)
               SEGMENT (PAUTSUM0)
               INTO (PENDING-AUTH-SUMMARY)
               WHERE (ACCNTID = PA-ACCT-ID)
           END-EXEC

           MOVE DIBSTAT                          TO IMS-RETURN-CODE
           EVALUATE TRUE
               WHEN STATUS-OK
                  SET FOUND-PAUT-SMRY-SEG        TO TRUE
               WHEN SEGMENT-NOT-FOUND
                  SET NFOUND-PAUT-SMRY-SEG       TO TRUE
               WHEN OTHER
                  MOVE 'Y'     TO WS-ERR-FLG

                  STRING
                  ' System error while reading AUTH Summary: Code:'
                  IMS-RETURN-CODE
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  MOVE -1       TO ACCTIDL OF COPAU0AI
                  PERFORM SEND-PAULST-SCREEN
           END-EVALUATE
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
              MOVE -1       TO ACCTIDL OF COPAU0AI
              PERFORM SEND-PAULST-SCREEN
           END-IF
           .

