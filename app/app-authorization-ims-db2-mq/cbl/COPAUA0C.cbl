      ******************************************************************
      * Program     : COPAUA0C.CBL
      * Application : CardDemo - Authorization Module
      * Type        : CICS COBOL IMS MQ Program
      * Function    : Card Authorization Decision Program
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
       PROGRAM-ID. COPAUA0C.                                                    
       AUTHOR.     SOUMA GHOSH.                                                 
                                                                                
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
                                                                                
       DATA DIVISION.                                                           
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01 WS-VARIABLES.                                                         
         05 WS-PGM-AUTH                PIC X(08)  VALUE 'COPAUA0C'.             
         05 WS-CICS-TRANID             PIC X(04)  VALUE 'CP00'.                 
         05 WS-ACCTFILENAME            PIC X(8)   VALUE 'ACCTDAT '.             
         05 WS-CUSTFILENAME            PIC X(8)   VALUE 'CUSTDAT '.             
         05 WS-CARDFILENAME            PIC X(8)   VALUE 'CARDDAT '.             
         05 WS-CARDFILENAME-ACCT-PATH  PIC X(8)   VALUE 'CARDAIX '.             
         05 WS-CCXREF-FILE             PIC X(08)  VALUE 'CCXREF  '.             
         05 WS-REQSTS-PROCESS-LIMIT    PIC S9(4)  COMP VALUE 500.               
                                                                                
         05 WS-MSG-PROCESSED           PIC S9(4)  COMP VALUE ZERO.              
         05 WS-REQUEST-QNAME           PIC X(48).                               
         05 WS-REPLY-QNAME             PIC X(48).                               
         05 WS-SAVE-CORRELID           PIC X(24).                               
         05 WS-RESP-LENGTH             PIC S9(4)  VALUE 1.                      
         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.             
         05 WS-REAS-CD                 PIC S9(09) COMP VALUE ZEROS.             
                                                                                
         05 WS-ABS-TIME                PIC S9(15) COMP-3 VALUE 0.               
         05 WS-CUR-DATE-X6             PIC X(06)  VALUE SPACES.                 
         05 WS-CUR-TIME-X6             PIC X(06)  VALUE SPACES.                 
         05 WS-CUR-TIME-N6             PIC 9(06)  VALUE ZERO.                   
         05 WS-CUR-TIME-MS             PIC S9(08) COMP.                         
         05 WS-YYDDD                   PIC 9(05).                               
         05 WS-TIME-WITH-MS            PIC S9(09) COMP-3.                       
         05 WS-OPTIONS                 PIC S9(9)  BINARY.                       
         05 WS-COMPCODE                PIC S9(9)  BINARY.                       
         05 WS-REASON                  PIC S9(9)  BINARY.                       
         05 WS-WAIT-INTERVAL           PIC S9(9)  BINARY.                       
         05 WS-CODE-DISPLAY            PIC 9(9).                                
         05 WS-AVAILABLE-AMT           PIC S9(09)V99 COMP-3.                    
         05 WS-TRANSACTION-AMT-AN      PIC X(13).                               
         05 WS-TRANSACTION-AMT         PIC S9(10)V99.                           
         05 WS-APPROVED-AMT            PIC S9(10)V99.                           
         05 WS-APPROVED-AMT-DIS        PIC -zzzzzzzzz9.99.                      
         05 WS-TRIGGER-DATA            PIC X(64).                               
                                                                                
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
                                                                                
       01  W01-HCONN-REQUEST           PIC S9(9) BINARY VALUE ZERO.             
       01  W01-HOBJ-REQUEST            PIC S9(9) BINARY.                        
       01  W01-BUFFLEN                 PIC S9(9) BINARY.                        
       01  W01-DATALEN                 PIC S9(9) BINARY.                        
       01  W01-GET-BUFFER              PIC X(500).                              
                                                                                
       01  W02-HCONN-REPLY             PIC S9(9) BINARY VALUE ZERO.             
       01  W02-BUFFLEN                 PIC S9(9) BINARY.                        
       01  W02-DATALEN                 PIC S9(9) BINARY.                        
       01  W02-PUT-BUFFER              PIC X(200).                              
                                                                                
       01  WS-SWITCHES.                                                         
           05 WS-AUTH-RESP-FLG         PIC X(01).                               
              88 AUTH-RESP-APPROVED    VALUE 'A'.                               
              88 AUTH-RESP-DECLINED    VALUE 'D'.                               
           05 WS-MSG-LOOP-FLG          PIC X(01) VALUE 'N'.                     
              88 WS-LOOP-END           VALUE 'E'.                               
           05 WS-MSG-AVAILABLE-FLG     PIC X(01) VALUE 'M'.                     
              88 NO-MORE-MSG-AVAILABLE VALUE 'N'.                               
              88 MORE-MSG-AVAILABLE    VALUE 'M'.                               
           05 WS-REQUEST-MQ-FLG        PIC X(01) VALUE 'C'.                     
              88 WS-REQUEST-MQ-OPEN    VALUE 'O'.                               
              88 WS-REQUEST-MQ-CLSE    VALUE 'C'.                               
           05 WS-REPLY-MQ-FLG          PIC X(01) VALUE 'C'.                     
              88 WS-REPLY-MQ-OPEN      VALUE 'O'.                               
              88 WS-REPLY-MQ-CLSE      VALUE 'C'.                               
           05 WS-XREF-READ-FLG         PIC X(1).                                
              88 CARD-NFOUND-XREF      VALUE 'N'.                               
              88 CARD-FOUND-XREF       VALUE 'Y'.                               
           05 WS-ACCT-MASTER-READ-FLG PIC X(1).                                 
              88 FOUND-ACCT-IN-MSTR    VALUE 'Y'.                               
              88 NFOUND-ACCT-IN-MSTR   VALUE 'N'.                               
           05 WS-CUST-MASTER-READ-FLG PIC X(1).                                 
              88 FOUND-CUST-IN-MSTR    VALUE 'Y'.                               
              88 NFOUND-CUST-IN-MSTR   VALUE 'N'.                               
           05 WS-PAUT-SMRY-SEG-FLG     PIC X(1).                                
              88 FOUND-PAUT-SMRY-SEG   VALUE 'Y'.                               
              88 NFOUND-PAUT-SMRY-SEG  VALUE 'N'.                               
           05 WS-DECLINE-FLG           PIC X(1).                                
              88 APPROVE-AUTH          VALUE 'A'.                               
              88 DECLINE-AUTH          VALUE 'D'.                               
           05 WS-DECLINE-REASON-FLG    PIC X(1).                                
              88 INSUFFICIENT-FUND     VALUE 'I'.                               
              88 CARD-NOT-ACTIVE       VALUE 'A'.                               
              88 ACCOUNT-CLOSED        VALUE 'C'.                               
              88 CARD-FRAUD            VALUE 'F'.                               
              88 MERCHANT-FRAUD        VALUE 'M'.                               
                                                                                
                                                                                
       01  MQM-OD-REQUEST.                                                      
           COPY CMQODV.                                                         
                                                                                
       01  MQM-MD-REQUEST.                                                      
           COPY CMQMDV.                                                         
                                                                                
       01  MQM-OD-REPLY.                                                        
           COPY CMQODV.                                                         
                                                                                
       01  MQM-MD-REPLY.                                                        
           COPY CMQMDV.                                                         
                                                                                
       01  MQM-CONSTANTS.                                                       
           COPY CMQV.                                                           
                                                                                
       01  MQM-TRIGGER-DATA.                                                    
           COPY CMQTML.                                                         
                                                                                
       01  MQM-PUT-MESSAGE-OPTIONS.                                             
           COPY CMQPMOV.                                                        
                                                                                
       01  MQM-GET-MESSAGE-OPTIONS.                                             
           COPY CMQGMOV.                                                        
                                                                                
      *----------------------------------------------------------------*        
      *  STAGING COPYBOOKS                                                      
      *----------------------------------------------------------------*        
                                                                                
      *- PENDING AUTHORIZATION REQUEST LAYOUT                                   
       01 PENDING-AUTH-REQUEST.                                                 
       COPY CCPAURQY.                                                           
                                                                                
      *- PENDING AUTHORIZATION RESPONSE LAYOUT                                  
       01 PENDING-AUTH-RESPONSE.                                                
       COPY CCPAURLY.                                                           
                                                                                
      *- APPLICTION ERROR LOG LAYOUT                                            
       COPY CCPAUERY.                                                           
                                                                                
      *----------------------------------------------------------------*        
      *  IMS SEGMENT LAYOUT                                                     
      *----------------------------------------------------------------*        
                                                                                
      *- PENDING AUTHORIZATION SUMMARY SEGMENT - ROOT                           
       01 PENDING-AUTH-SUMMARY.                                                 
       COPY CIPAUSMY.                                                           
                                                                                
      *- PENDING AUTHORIZATION DETAILS SEGMENT - CHILD                          
       01 PENDING-AUTH-DETAILS.                                                 
       COPY CIPAUDTY.                                                           
                                                                                
      *----------------------------------------------------------------*        
      *DATASET LAYOUTS                                                          
      *----------------------------------------------------------------*        
      *- CARD XREF LAYOUT                                                       
       COPY CVACT03Y.                                                           
                                                                                
      *- ACCT RECORD LAYOUT                                                     
       COPY CVACT01Y.                                                           
                                                                                
      *- CUSTOMER LAYOUT                                                        
       COPY CVCUS01Y.                                                           
                                                                                
      * ------------------------------------------------------------- *         
       LINKAGE SECTION.                                                         
      * ------------------------------------------------------------- *         
       01  DFHCOMMAREA.                                                         
         05  LK-COMMAREA                           PIC X(4096).                 
                                                                                
      * ------------------------------------------------------------- *         
       PROCEDURE DIVISION.                                                      
      * ------------------------------------------------------------- *         
       MAIN-PARA.                                                               
                                                                                
           PERFORM 1000-INITIALIZE    THRU 1000-EXIT                            
           PERFORM 2000-MAIN-PROCESS  THRU 2000-EXIT                            
           PERFORM 9000-TERMINATE     THRU 9000-EXIT                            
                                                                                
           EXEC CICS RETURN                                                     
           END-EXEC.                                                            
                                                                                
      * ------------------------------------------------------------- *         
       1000-INITIALIZE.                                                         
      * ------------------------------------------------------------- *         
      *                                                                         
           EXEC CICS RETRIEVE                                                   
             INTO(MQTM)                                                         
             NOHANDLE                                                           
           END-EXEC                                                             
           IF EIBRESP = DFHRESP(NORMAL)                                         
              MOVE MQTM-QNAME              TO WS-REQUEST-QNAME                  
              MOVE MQTM-TRIGGERDATA        TO WS-TRIGGER-DATA                   
           END-IF                                                               
                                                                                
           MOVE 5000                       TO WS-WAIT-INTERVAL                  
                                                                                
           PERFORM 1100-OPEN-REQUEST-QUEUE THRU 1100-EXIT                       
                                                                                
           PERFORM 3100-READ-REQUEST-MQ    THRU 3100-EXIT                       
           .                                                                    
      *                                                                         
       1000-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
      * ------------------------------------------------------------- *         
      *  OPEN THE REQUEST QUEUE                                       *         
      * ------------------------------------------_------------------ *         
       1100-OPEN-REQUEST-QUEUE.                                                 
      *                                                                         
           MOVE MQOT-Q             TO MQOD-OBJECTTYPE OF MQM-OD-REQUEST         
           MOVE WS-REQUEST-QNAME   TO MQOD-OBJECTNAME OF MQM-OD-REQUEST         
      *                                                                         
           COMPUTE WS-OPTIONS = MQOO-INPUT-SHARED                               
      *                                                                         
           CALL 'MQOPEN' USING W01-HCONN-REQUEST                                
                               MQM-OD-REQUEST                                   
                               WS-OPTIONS                                       
                               W01-HOBJ-REQUEST                                 
                               WS-COMPCODE                                      
                               WS-REASON                                        
           END-CALL                                                             
      *                                                                         
           IF WS-COMPCODE = MQCC-OK                                             
              SET WS-REQUEST-MQ-OPEN TO TRUE                                    
           ELSE                                                                 
              MOVE 'M001'          TO ERR-LOCATION                              
              SET  ERR-CRITICAL    TO TRUE                                      
              SET  ERR-MQ          TO TRUE                                      
              MOVE WS-COMPCODE     TO WS-CODE-DISPLAY                           
              MOVE WS-CODE-DISPLAY TO ERR-CODE-1                                
              MOVE WS-REASON       TO WS-CODE-DISPLAY                           
              MOVE WS-CODE-DISPLAY TO ERR-CODE-2                                
              MOVE 'REQ MQ OPEN ERROR'                                          
                                   TO ERR-MESSAGE                               
              PERFORM 9500-LOG-ERROR                                            
           END-IF                                                               
           .                                                                    
      *                                                                         
       1100-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
      * ------------------------------------------------------------- *         
      * SCHEDULE PSB                                                  * 08470000
      * ------------------------------------------------------------- *         
       1200-SCHEDULE-PSB.                                               08490000
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
              MOVE 'I001'                TO ERR-LOCATION                        
              SET  ERR-CRITICAL          TO TRUE                                
              SET  ERR-IMS               TO TRUE                                
              MOVE IMS-RETURN-CODE       TO ERR-CODE-1                          
              MOVE 'IMS SCHD FAILED'     TO ERR-MESSAGE                         
              PERFORM 9500-LOG-ERROR                                            
           END-IF                                                               
           .
       1200-EXIT.
           EXIT
           .
      * ------------------------------------------------------------- *         
       2000-MAIN-PROCESS.                                                       
      * ------------------------------------------------------------- *         
      *                                                                         
           PERFORM UNTIL NO-MORE-MSG-AVAILABLE OR WS-LOOP-END                   
                                                                                
             PERFORM 2100-EXTRACT-REQUEST-MSG THRU 2100-EXIT                    
                                                                                
             PERFORM 5000-PROCESS-AUTH        THRU 5000-EXIT                    
                                                                                
             ADD 1                            TO WS-MSG-PROCESSED               
                                                                                
             EXEC CICS                                                          
                  SYNCPOINT                                                     
             END-EXEC                                                           
             SET IMS-PSB-NOT-SCHD            TO TRUE                            
                                                                                
             IF WS-MSG-PROCESSED > WS-REQSTS-PROCESS-LIMIT                      
                SET  WS-LOOP-END             TO TRUE                            
             ELSE                                                               
                PERFORM 3100-READ-REQUEST-MQ THRU 3100-EXIT                     
             END-IF                                                             
           END-PERFORM                                                          
           .                                                                    
      *                                                                         
       2000-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
      * ------------------------------------------------------------- *         
       2100-EXTRACT-REQUEST-MSG.                                                
      * ------------------------------------------------------------- *         
      *                                                                         
           UNSTRING W01-GET-BUFFER(1:W01-DATALEN)                               
                    DELIMITED BY ','                                            
                    INTO PA-RQ-AUTH-DATE                                        
                         PA-RQ-AUTH-TIME                                        
                         PA-RQ-CARD-NUM                                         
                         PA-RQ-AUTH-TYPE                                        
                         PA-RQ-CARD-EXPIRY-DATE                                 
                         PA-RQ-MESSAGE-TYPE                                     
                         PA-RQ-MESSAGE-SOURCE                                   
                         PA-RQ-PROCESSING-CODE                                  
                         WS-TRANSACTION-AMT-AN                                  
                         PA-RQ-MERCHANT-CATAGORY-CODE                           
                         PA-RQ-ACQR-COUNTRY-CODE                                
                         PA-RQ-POS-ENTRY-MODE                                   
                         PA-RQ-MERCHANT-ID                                      
                         PA-RQ-MERCHANT-NAME                                    
                         PA-RQ-MERCHANT-CITY                                    
                         PA-RQ-MERCHANT-STATE                                   
                         PA-RQ-MERCHANT-ZIP                                     
                         PA-RQ-TRANSACTION-ID                                   
           END-UNSTRING                                                         
                                                                                
           COMPUTE PA-RQ-TRANSACTION-AMT =                                      
                               FUNCTION NUMVAL(WS-TRANSACTION-AMT-AN)           
                                                                                
           MOVE PA-RQ-TRANSACTION-AMT  TO WS-TRANSACTION-AMT                    
           .                                                                    
      *                                                                         
       2100-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
      * ------------------------------------------------------------- *         
       3100-READ-REQUEST-MQ.                                                    
      * ------------------------------------------------------------- *         
      *                                                                         
           COMPUTE MQGMO-OPTIONS  =  MQGMO-NO-SYNCPOINT + MQGMO-WAIT            
                                  +  MQGMO-CONVERT                              
                                  +  MQGMO-FAIL-IF-QUIESCING                    
                                                                                
           MOVE WS-WAIT-INTERVAL      TO MQGMO-WAITINTERVAL                     
                                                                                
           MOVE MQMI-NONE             TO MQMD-MSGID    OF MQM-MD-REQUEST        
           MOVE MQCI-NONE             TO MQMD-CORRELID OF MQM-MD-REQUEST        
           MOVE MQFMT-STRING          TO MQMD-FORMAT   OF MQM-MD-REQUEST        
           MOVE LENGTH OF W01-GET-BUFFER TO W01-BUFFLEN                         
                                                                                
           CALL 'MQGET' USING W01-HCONN-REQUEST                                 
                              W01-HOBJ-REQUEST                                  
                              MQM-MD-REQUEST                                    
                              MQM-GET-MESSAGE-OPTIONS                           
                              W01-BUFFLEN                                       
                              W01-GET-BUFFER                                    
                              W01-DATALEN                                       
                              WS-COMPCODE                                       
                              WS-REASON                                         
           END-CALL                                                             
           IF WS-COMPCODE = MQCC-OK                                             
              MOVE MQMD-CORRELID OF MQM-MD-REQUEST                              
                                           TO WS-SAVE-CORRELID                  
              MOVE MQMD-REPLYTOQ OF MQM-MD-REQUEST                              
                                           TO WS-REPLY-QNAME                    
           ELSE                                                                 
              IF WS-REASON = MQRC-NO-MSG-AVAILABLE                              
                 SET NO-MORE-MSG-AVAILABLE TO TRUE                              
              ELSE                                                              
                MOVE 'M003'                TO ERR-LOCATION                      
                SET  ERR-CRITICAL          TO TRUE                              
                SET  ERR-CICS              TO TRUE                              
                MOVE WS-COMPCODE           TO WS-CODE-DISPLAY                   
                MOVE WS-CODE-DISPLAY       TO ERR-CODE-1                        
                MOVE WS-REASON             TO WS-CODE-DISPLAY                   
                MOVE WS-CODE-DISPLAY       TO ERR-CODE-2                        
                MOVE 'FAILED TO READ REQUEST MQ'                                
                                           TO ERR-MESSAGE                       
                MOVE PA-CARD-NUM           TO ERR-EVENT-KEY                     
                PERFORM 9500-LOG-ERROR                                          
              END-IF                                                            
           END-IF                                                               
           .                                                                    
      *                                                                         
       3100-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
      * ------------------------------------------------------------- *         
       5000-PROCESS-AUTH.                                                       
      * ------------------------------------------------------------- *         
      *                                                                         
           SET APPROVE-AUTH                  TO TRUE                            
                                                                                
           PERFORM 1200-SCHEDULE-PSB         THRU 1200-EXIT                     
                                                                                
           SET CARD-FOUND-XREF               TO TRUE                            
           SET FOUND-ACCT-IN-MSTR            TO TRUE                            
                                                                                
           PERFORM 5100-READ-XREF-RECORD     THRU 5100-EXIT                     
                                                                                
           IF CARD-FOUND-XREF                                                   
              PERFORM 5200-READ-ACCT-RECORD  THRU 5200-EXIT                     
              PERFORM 5300-READ-CUST-RECORD  THRU 5300-EXIT                     
                                                                                
              PERFORM 5500-READ-AUTH-SUMMRY  THRU 5500-EXIT                     
                                                                                
              PERFORM 5600-READ-PROFILE-DATA THRU 5600-EXIT                     
           END-IF                                                               
                                                                                
           PERFORM 6000-MAKE-DECISION        THRU 6000-EXIT                     
                                                                                
           PERFORM 7100-SEND-RESPONSE        THRU 7100-EXIT                     
                                                                                
           IF CARD-FOUND-XREF                                                   
              PERFORM 8000-WRITE-AUTH-TO-DB  THRU 8000-EXIT                     
           END-IF                                                               
           .                                                                    
      *                                                                         
       5000-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
      * ------------------------------------------------------------- *         
       5100-READ-XREF-RECORD.                                                   
      * ------------------------------------------------------------- *         
      *                                                                         
           MOVE PA-RQ-CARD-NUM           TO XREF-CARD-NUM                       
                                                                                
           EXEC CICS READ                                                       
                DATASET   (WS-CCXREF-FILE)                                      
                INTO      (CARD-XREF-RECORD)                                    
                LENGTH    (LENGTH OF CARD-XREF-RECORD)                          
                RIDFLD    (XREF-CARD-NUM)                                       
                KEYLENGTH (LENGTH OF XREF-CARD-NUM)                             
                RESP      (WS-RESP-CD)                                          
                RESP2     (WS-REAS-CD)                                          
           END-EXEC                                                             
                                                                                
           EVALUATE WS-RESP-CD                                                  
               WHEN DFHRESP(NORMAL)                                             
                   SET  CARD-FOUND-XREF  TO TRUE                                
               WHEN DFHRESP(NOTFND)                                             
                   SET  CARD-NFOUND-XREF TO TRUE                                
                   SET  NFOUND-ACCT-IN-MSTR TO TRUE                             
                                                                                
                   MOVE 'A001'          TO ERR-LOCATION                         
                   SET  ERR-WARNING     TO TRUE                                 
                   SET  ERR-APP         TO TRUE                                 
                   MOVE 'CARD NOT FOUND IN XREF'                                
                                        TO ERR-MESSAGE                          
                   MOVE XREF-CARD-NUM   TO ERR-EVENT-KEY                        
                   PERFORM 9500-LOG-ERROR                                       
               WHEN OTHER                                                       
                   MOVE 'C001'          TO ERR-LOCATION                         
                   SET  ERR-CRITICAL    TO TRUE                                 
                   SET  ERR-CICS        TO TRUE                                 
                   MOVE WS-RESP-CD      TO WS-CODE-DISPLAY                      
                   MOVE WS-CODE-DISPLAY TO ERR-CODE-1                           
                   MOVE WS-REAS-CD      TO WS-CODE-DISPLAY                      
                   MOVE WS-CODE-DISPLAY TO ERR-CODE-2                           
                   MOVE 'FAILED TO READ XREF FILE'                              
                                        TO ERR-MESSAGE                          
                   MOVE XREF-CARD-NUM   TO ERR-EVENT-KEY                        
                   PERFORM 9500-LOG-ERROR                                       
           END-EVALUATE                                                         
           .                                                                    
      *                                                                         
       5100-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
      * ------------------------------------------------------------- *         
       5200-READ-ACCT-RECORD.                                                   
      * ------------------------------------------------------------- *         
      *                                                                         
           MOVE XREF-ACCT-ID          TO WS-CARD-RID-ACCT-ID                    
                                                                                
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
                  SET FOUND-ACCT-IN-MSTR     TO TRUE                            
               WHEN DFHRESP(NOTFND)                                             
                  SET NFOUND-ACCT-IN-MSTR    TO TRUE                            
                                                                                
                  MOVE 'A002'                TO ERR-LOCATION                    
                  SET  ERR-WARNING           TO TRUE                            
                  SET  ERR-APP               TO TRUE                            
                  MOVE 'ACCT NOT FOUND IN XREF'                                 
                                             TO ERR-MESSAGE                     
                  MOVE WS-CARD-RID-ACCT-ID-X TO ERR-EVENT-KEY                   
                  PERFORM 9500-LOG-ERROR                                        
      *                                                                         
               WHEN OTHER                                                       
                  MOVE 'C002'                TO ERR-LOCATION                    
                  SET  ERR-CRITICAL          TO TRUE                            
                  SET  ERR-CICS              TO TRUE                            
                  MOVE WS-RESP-CD            TO WS-CODE-DISPLAY                 
                  MOVE WS-CODE-DISPLAY       TO ERR-CODE-1                      
                  MOVE WS-REAS-CD            TO WS-CODE-DISPLAY                 
                  MOVE WS-CODE-DISPLAY       TO ERR-CODE-2                      
                  MOVE 'FAILED TO READ ACCT FILE'                               
                                             TO ERR-MESSAGE                     
                  MOVE WS-CARD-RID-ACCT-ID-X TO ERR-EVENT-KEY                   
                  PERFORM 9500-LOG-ERROR                                        
           END-EVALUATE                                                         
           .                                                                    
      *                                                                         
       5200-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
      * ------------------------------------------------------------- *         
       5300-READ-CUST-RECORD.                                                   
      * ------------------------------------------------------------- *         
      *                                                                         
           MOVE XREF-CUST-ID                 TO WS-CARD-RID-CUST-ID             
                                                                                
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
                  SET FOUND-CUST-IN-MSTR     TO TRUE                            
               WHEN DFHRESP(NOTFND)                                             
                  SET NFOUND-CUST-IN-MSTR    TO TRUE                            
                                                                                
                  MOVE 'A003'                TO ERR-LOCATION                    
                  SET  ERR-WARNING           TO TRUE                            
                  SET  ERR-APP               TO TRUE                            
                  MOVE 'CUST NOT FOUND IN XREF'                                 
                                             TO ERR-MESSAGE                     
                  MOVE WS-CARD-RID-CUST-ID   TO ERR-EVENT-KEY                   
                  PERFORM 9500-LOG-ERROR                                        
      *                                                                         
               WHEN OTHER                                                       
                  MOVE 'C003'                TO ERR-LOCATION                    
                  SET  ERR-CRITICAL          TO TRUE                            
                  SET  ERR-CICS              TO TRUE                            
                  MOVE WS-RESP-CD            TO WS-CODE-DISPLAY                 
                  MOVE WS-CODE-DISPLAY       TO ERR-CODE-1                      
                  MOVE WS-REAS-CD            TO WS-CODE-DISPLAY                 
                  MOVE WS-CODE-DISPLAY       TO ERR-CODE-2                      
                  MOVE 'FAILED TO READ CUST FILE'                               
                                             TO ERR-MESSAGE                     
                  MOVE WS-CARD-RID-CUST-ID   TO ERR-EVENT-KEY                   
                  PERFORM 9500-LOG-ERROR                                        
           END-EVALUATE                                                         
           .                                                                    
      *                                                                         
       5300-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
      * ------------------------------------------------------------- *         
       5500-READ-AUTH-SUMMRY.                                                   
      * ------------------------------------------------------------- *         
      *                                                                         
           MOVE XREF-ACCT-ID                    TO PA-ACCT-ID                   
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
                  MOVE 'I002'                    TO ERR-LOCATION                
                  SET  ERR-CRITICAL              TO TRUE                        
                  SET  ERR-IMS                   TO TRUE                        
                  MOVE IMS-RETURN-CODE           TO ERR-CODE-1                  
                  MOVE 'IMS GET SUMMARY FAILED'  TO ERR-MESSAGE                 
                  MOVE PA-CARD-NUM               TO ERR-EVENT-KEY               
                  PERFORM 9500-LOG-ERROR                                        
           END-EVALUATE                                                         
           .                                                                    
      *                                                                         
       5500-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
      * ------------------------------------------------------------- *         
       5600-READ-PROFILE-DATA.                                                  
      * ------------------------------------------------------------- *         
      *                                                                         
           CONTINUE                                                             
           .                                                                    
      *                                                                         
       5600-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
      * ------------------------------------------------------------- *         
       6000-MAKE-DECISION.                                                      
      * ------------------------------------------------------------- *         
      *                                                                         
           MOVE PA-RQ-CARD-NUM         TO PA-RL-CARD-NUM                        
           MOVE PA-RQ-TRANSACTION-ID   TO PA-RL-TRANSACTION-ID                  
           MOVE PA-RQ-AUTH-TIME        TO PA-RL-AUTH-ID-CODE                    
                                                                                
      *-   Decline Auth if Above Limit; If no AUTH summary, use ACT data        
           IF FOUND-PAUT-SMRY-SEG                                               
              COMPUTE WS-AVAILABLE-AMT = PA-CREDIT-LIMIT                        
                                       - PA-CREDIT-BALANCE                      
              IF WS-TRANSACTION-AMT > WS-AVAILABLE-AMT                          
                 SET DECLINE-AUTH      TO TRUE                                  
                 SET INSUFFICIENT-FUND TO TRUE                                  
              END-IF                                                            
           ELSE                                                                 
              IF FOUND-ACCT-IN-MSTR                                             
                 COMPUTE WS-AVAILABLE-AMT = ACCT-CREDIT-LIMIT                   
                                          - ACCT-CURR-BAL                       
                 IF WS-TRANSACTION-AMT > WS-AVAILABLE-AMT                       
                    SET DECLINE-AUTH      TO TRUE                               
                    SET INSUFFICIENT-FUND TO TRUE                               
                 END-IF                                                         
              ELSE                                                              
                 SET DECLINE-AUTH         TO TRUE                               
              END-IF                                                            
           END-IF                                                               
                                                                                
           IF DECLINE-AUTH                                                      
              SET  AUTH-RESP-DECLINED     TO TRUE                               
                                                                                
              MOVE '05'                   TO PA-RL-AUTH-RESP-CODE               
              MOVE 0                      TO PA-RL-APPROVED-AMT                 
                                             WS-APPROVED-AMT                    
           ELSE                                                                 
              SET  AUTH-RESP-APPROVED     TO TRUE                               
              MOVE '00'                   TO PA-RL-AUTH-RESP-CODE               
              MOVE PA-RQ-TRANSACTION-AMT  TO PA-RL-APPROVED-AMT                 
                                             WS-APPROVED-AMT                    
           END-IF                                                               
                                                                                
           MOVE '0000'                    TO PA-RL-AUTH-RESP-REASON             
           IF AUTH-RESP-DECLINED                                                
              EVALUATE TRUE                                                     
                 WHEN CARD-NFOUND-XREF                                          
                 WHEN NFOUND-ACCT-IN-MSTR                                       
                 WHEN NFOUND-CUST-IN-MSTR                                       
                      MOVE '3100'         TO PA-RL-AUTH-RESP-REASON             
                 WHEN INSUFFICIENT-FUND                                         
                      MOVE '4100'         TO PA-RL-AUTH-RESP-REASON             
                 WHEN CARD-NOT-ACTIVE                                           
                      MOVE '4200'         TO PA-RL-AUTH-RESP-REASON             
                 WHEN ACCOUNT-CLOSED                                            
                      MOVE '4300'         TO PA-RL-AUTH-RESP-REASON             
                 WHEN CARD-FRAUD                                                
                      MOVE '5100'         TO PA-RL-AUTH-RESP-REASON             
                 WHEN MERCHANT-FRAUD                                            
                      MOVE '5200'         TO PA-RL-AUTH-RESP-REASON             
                 WHEN OTHER                                                     
                      MOVE '9000'         TO PA-RL-AUTH-RESP-REASON             
              END-EVALUATE                                                      
           END-IF                                                               
                                                                                
           MOVE WS-APPROVED-AMT        TO WS-APPROVED-AMT-DIS                   
                                                                                
           STRING PA-RL-CARD-NUM         ','                                    
                  PA-RL-TRANSACTION-ID   ','                                    
                  PA-RL-AUTH-ID-CODE     ','                                    
                  PA-RL-AUTH-RESP-CODE   ','                                    
                  PA-RL-AUTH-RESP-REASON ','                                    
                  WS-APPROVED-AMT-DIS    ','                                    
                  DELIMITED BY SIZE                                             
                  INTO W02-PUT-BUFFER                                           
                  WITH POINTER WS-RESP-LENGTH                                   
           END-STRING                                                           
           .                                                                    
      *                                                                         
       6000-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
      * ------------------------------------------------------------- *         
       7100-SEND-RESPONSE.                                                      
      * ------------------------------------------------------------- *         
      *                                                                         
           MOVE MQOT-Q               TO MQOD-OBJECTTYPE OF MQM-OD-REPLY         
           MOVE WS-REPLY-QNAME       TO MQOD-OBJECTNAME OF MQM-OD-REPLY         
      *                                                                         
           MOVE MQMT-REPLY           TO MQMD-MSGTYPE     OF MQM-MD-REPLY        
           MOVE WS-SAVE-CORRELID     TO MQMD-CORRELID    OF MQM-MD-REPLY        
           MOVE MQMI-NONE            TO MQMD-MSGID       OF MQM-MD-REPLY        
           MOVE SPACES               TO MQMD-REPLYTOQ    OF MQM-MD-REPLY        
           MOVE SPACES               TO MQMD-REPLYTOQMGR OF MQM-MD-REPLY        
           MOVE MQPER-NOT-PERSISTENT TO MQMD-PERSISTENCE OF MQM-MD-REPLY        
           MOVE 50                   TO MQMD-EXPIRY      OF MQM-MD-REPLY        
           MOVE MQFMT-STRING         TO MQMD-FORMAT      OF MQM-MD-REPLY        
                                                                                
           COMPUTE MQPMO-OPTIONS     =  MQPMO-NO-SYNCPOINT +                    
                                        MQPMO-DEFAULT-CONTEXT                   
                                                                                
           MOVE WS-RESP-LENGTH       TO W02-BUFFLEN                             
      *                                                                         
           CALL 'MQPUT1' USING W02-HCONN-REPLY                                  
                               MQM-OD-REPLY                                     
                               MQM-MD-REPLY                                     
                               MQM-PUT-MESSAGE-OPTIONS                          
                               W02-BUFFLEN                                      
                               W02-PUT-BUFFER                                   
                               WS-COMPCODE                                      
                               WS-REASON                                        
           END-CALL                                                             
           IF WS-COMPCODE NOT = MQCC-OK                                         
              MOVE 'M004'                TO ERR-LOCATION                        
              SET  ERR-CRITICAL          TO TRUE                                
              SET  ERR-MQ                TO TRUE                                
              MOVE WS-COMPCODE           TO WS-CODE-DISPLAY                     
              MOVE WS-CODE-DISPLAY       TO ERR-CODE-1                          
              MOVE WS-REASON             TO WS-CODE-DISPLAY                     
              MOVE WS-CODE-DISPLAY       TO ERR-CODE-2                          
              MOVE 'FAILED TO PUT ON REPLY MQ'                                  
                                         TO ERR-MESSAGE                         
              MOVE PA-CARD-NUM           TO ERR-EVENT-KEY                       
              PERFORM 9500-LOG-ERROR                                            
           END-IF                                                               
           .                                                                    
      *                                                                         
       7100-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
      * ------------------------------------------------------------- *         
       8000-WRITE-AUTH-TO-DB.                                                   
      * ------------------------------------------------------------- *         
      *                                                                         
                                                                                
           PERFORM 8400-UPDATE-SUMMARY      THRU 8400-EXIT                      
           PERFORM 8500-INSERT-AUTH         THRU 8500-EXIT                      
           .                                                                    
      *                                                                         
       8000-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
      * ------------------------------------------------------------- *         
       8400-UPDATE-SUMMARY.                                                     
      * ------------------------------------------------------------- *         
      *                                                                         
           IF NFOUND-PAUT-SMRY-SEG                                              
              INITIALIZE PENDING-AUTH-SUMMARY                                   
                REPLACING NUMERIC DATA BY ZERO                                  
                                                                                
              MOVE XREF-ACCT-ID             TO PA-ACCT-ID                       
              MOVE XREF-CUST-ID             TO PA-CUST-ID                       
                                                                                
           END-IF                                                               
                                                                                
           MOVE ACCT-CREDIT-LIMIT           TO PA-CREDIT-LIMIT                  
           MOVE ACCT-CASH-CREDIT-LIMIT      TO PA-CASH-LIMIT                    
                                                                                
           IF AUTH-RESP-APPROVED                                                
              ADD 1                         TO PA-APPROVED-AUTH-CNT             
              ADD WS-APPROVED-AMT           TO PA-APPROVED-AUTH-AMT             
                                                                                
              ADD WS-APPROVED-AMT           TO PA-CREDIT-BALANCE                
              MOVE 0                        TO PA-CASH-BALANCE                  
           ELSE                                                                 
              ADD 1                         TO PA-DECLINED-AUTH-CNT             
              ADD PA-TRANSACTION-AMT        TO PA-DECLINED-AUTH-AMT             
           END-IF                                                               
                                                                                
           IF FOUND-PAUT-SMRY-SEG                                               
              EXEC DLI REPL USING PCB(PAUT-PCB-NUM)                             
                   SEGMENT (PAUTSUM0)                                           
                   FROM (PENDING-AUTH-SUMMARY)                                  
              END-EXEC                                                          
           ELSE                                                                 
              EXEC DLI ISRT USING PCB(PAUT-PCB-NUM)                             
                   SEGMENT (PAUTSUM0)                                           
                   FROM (PENDING-AUTH-SUMMARY)                                  
              END-EXEC                                                          
           END-IF                                                               
           MOVE DIBSTAT                     TO IMS-RETURN-CODE                  
                                                                                
           IF STATUS-OK                                                         
             CONTINUE                                                           
           ELSE                                                                 
             MOVE 'I003'                    TO ERR-LOCATION                     
             SET  ERR-CRITICAL              TO TRUE                             
             SET  ERR-IMS                   TO TRUE                             
             MOVE IMS-RETURN-CODE           TO ERR-CODE-1                       
             MOVE 'IMS UPDATE SUMRY FAILED' TO ERR-MESSAGE                      
             MOVE PA-CARD-NUM               TO ERR-EVENT-KEY                    
             PERFORM 9500-LOG-ERROR                                             
           END-IF                                                               
           .                                                                    
      *                                                                         
       8400-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
      * ------------------------------------------------------------- *         
       8500-INSERT-AUTH.                                                        
      * ------------------------------------------------------------- *         
      *                                                                         
           EXEC CICS ASKTIME NOHANDLE                                           
              ABSTIME(WS-ABS-TIME)                                              
           END-EXEC                                                             
                                                                                
           EXEC CICS FORMATTIME                                                 
             ABSTIME(WS-ABS-TIME)                                               
             YYDDD(WS-CUR-DATE-X6)                                              
             TIME(WS-CUR-TIME-X6)                                               
             MILLISECONDS(WS-CUR-TIME-MS)                                       
           END-EXEC                                                             
                                                                                
           MOVE WS-CUR-DATE-X6(1:5)         TO WS-YYDDD                         
           MOVE WS-CUR-TIME-X6              TO WS-CUR-TIME-N6                   
                                                                                
           COMPUTE WS-TIME-WITH-MS = (WS-CUR-TIME-N6 * 1000) +                  
                                     WS-CUR-TIME-MS                             
                                                                                
           COMPUTE PA-AUTH-DATE-9C = 99999 - WS-YYDDD                           
           COMPUTE PA-AUTH-TIME-9C = 999999999 - WS-TIME-WITH-MS                
                                                                                
           MOVE PA-RQ-AUTH-DATE             TO PA-AUTH-ORIG-DATE                
           MOVE PA-RQ-AUTH-TIME             TO PA-AUTH-ORIG-TIME                
           MOVE PA-RQ-CARD-NUM              TO PA-CARD-NUM                      
           MOVE PA-RQ-AUTH-TYPE             TO PA-AUTH-TYPE                     
           MOVE PA-RQ-CARD-EXPIRY-DATE      TO PA-CARD-EXPIRY-DATE              
           MOVE PA-RQ-MESSAGE-TYPE          TO PA-MESSAGE-TYPE                  
           MOVE PA-RQ-MESSAGE-SOURCE        TO PA-MESSAGE-SOURCE                
           MOVE PA-RQ-PROCESSING-CODE       TO PA-PROCESSING-CODE               
           MOVE PA-RQ-TRANSACTION-AMT       TO PA-TRANSACTION-AMT               
           MOVE PA-RQ-MERCHANT-CATAGORY-CODE                                    
                                            TO PA-MERCHANT-CATAGORY-CODE        
           MOVE PA-RQ-ACQR-COUNTRY-CODE     TO PA-ACQR-COUNTRY-CODE             
           MOVE PA-RQ-POS-ENTRY-MODE        TO PA-POS-ENTRY-MODE                
           MOVE PA-RQ-MERCHANT-ID           TO PA-MERCHANT-ID                   
           MOVE PA-RQ-MERCHANT-NAME         TO PA-MERCHANT-NAME                 
           MOVE PA-RQ-MERCHANT-CITY         TO PA-MERCHANT-CITY                 
           MOVE PA-RQ-MERCHANT-STATE        TO PA-MERCHANT-STATE                
           MOVE PA-RQ-MERCHANT-ZIP          TO PA-MERCHANT-ZIP                  
           MOVE PA-RQ-TRANSACTION-ID        TO PA-TRANSACTION-ID                
                                                                                
           MOVE PA-RL-AUTH-ID-CODE          TO PA-AUTH-ID-CODE                  
           MOVE PA-RL-AUTH-RESP-CODE        TO PA-AUTH-RESP-CODE                
           MOVE PA-RL-AUTH-RESP-REASON      TO PA-AUTH-RESP-REASON              
           MOVE PA-RL-APPROVED-AMT          TO PA-APPROVED-AMT                  
                                                                                
           IF AUTH-RESP-APPROVED                                                
              SET  PA-MATCH-PENDING         TO TRUE
           ELSE                                                                 
              SET  PA-MATCH-AUTH-DECLINED   TO TRUE
           END-IF                                                               

           MOVE SPACE                       TO PA-AUTH-FRAUD
                                               PA-FRAUD-RPT-DATE

           MOVE XREF-ACCT-ID                TO PA-ACCT-ID                       
                                                                                
           EXEC DLI ISRT USING PCB(PAUT-PCB-NUM)
                SEGMENT (PAUTSUM0)
                WHERE (ACCNTID = PA-ACCT-ID)
                SEGMENT (PAUTDTL1)
                FROM (PENDING-AUTH-DETAILS)
                SEGLENGTH (LENGTH OF PENDING-AUTH-DETAILS)
           END-EXEC
           MOVE DIBSTAT                     TO IMS-RETURN-CODE                  
                                                                                
           IF STATUS-OK                                                         
             CONTINUE                                                           
           ELSE                                                                 
             MOVE 'I004'                    TO ERR-LOCATION                     
             SET  ERR-CRITICAL              TO TRUE                             
             SET  ERR-IMS                   TO TRUE                             
             MOVE IMS-RETURN-CODE           TO ERR-CODE-1                       
             MOVE 'IMS INSERT DETL FAILED'  TO ERR-MESSAGE                      
             MOVE PA-CARD-NUM               TO ERR-EVENT-KEY                    
             PERFORM 9500-LOG-ERROR                                             
           END-IF                                                               
           .                                                                    
      *                                                                         
       8500-EXIT.                                                               
           EXIT.                                                                
      *                                                                         

      * ------------------------------------------------------------- *         
       9000-TERMINATE.                                                          
      * ------------------------------------------------------------- *         
      *                                                                         
           IF IMS-PSB-SCHD                                                      
              EXEC DLI TERM END-EXEC                                            
           END-IF                                                               
                                                                                
           PERFORM 9100-CLOSE-REQUEST-QUEUE THRU 9100-EXIT                      
           .                                                                    
      *                                                                         
       9000-EXIT.                                                               
           EXIT.                                                                
      * ------------------------------------------------------------- *         
       9100-CLOSE-REQUEST-QUEUE.                                                
      * ------------------------------------------------------------ *          
           IF WS-REQUEST-MQ-OPEN                                                
              CALL 'MQCLOSE' USING W01-HCONN-REQUEST                            
                                W01-HOBJ-REQUEST                                
                                MQCO-NONE                                       
                                WS-COMPCODE                                     
                                WS-REASON                                       
              END-CALL                                                          
      *                                                                         
              IF WS-COMPCODE = MQCC-OK                                          
                 SET WS-REQUEST-MQ-CLSE TO TRUE                                 
              ELSE                                                              
                 MOVE 'M005'                TO ERR-LOCATION                     
                 SET  ERR-WARNING           TO TRUE                             
                 SET  ERR-MQ                TO TRUE                             
                 MOVE WS-COMPCODE           TO WS-CODE-DISPLAY                  
                 MOVE WS-CODE-DISPLAY       TO ERR-CODE-1                       
                 MOVE WS-REASON             TO WS-CODE-DISPLAY                  
                 MOVE WS-CODE-DISPLAY       TO ERR-CODE-2                       
                 MOVE 'FAILED TO CLOSE REQUEST MQ'                              
                                            TO ERR-MESSAGE                      
                 PERFORM 9500-LOG-ERROR                                         
              END-IF                                                            
           END-IF.                                                              
      *                                                                         
       9100-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
      * ------------------------------------------------------------- *         
       9500-LOG-ERROR.                                                          
      * ------------------------------------------------------------ *          
                                                                                
           EXEC CICS ASKTIME NOHANDLE                                           
              ABSTIME(WS-ABS-TIME)                                              
           END-EXEC                                                             
                                                                                
           EXEC CICS FORMATTIME                                                 
             ABSTIME(WS-ABS-TIME)                                               
             YYMMDD(WS-CUR-DATE-X6)                                             
             TIME(WS-CUR-TIME-X6)                                               
           END-EXEC                                                             
                                                                                
           MOVE WS-CICS-TRANID            TO ERR-APPLICATION                    
           MOVE WS-PGM-AUTH               TO ERR-PROGRAM                        
           MOVE WS-CUR-DATE-X6            TO ERR-DATE                           
           MOVE WS-CUR-TIME-X6            TO ERR-TIME                           
                                                                                
           EXEC CICS WRITEQ                                                     
                TD QUEUE('CSSL')                                                
                FROM (ERROR-LOG-RECORD)                                         
                LENGTH (LENGTH OF ERROR-LOG-RECORD)                             
                NOHANDLE                                                        
           END-EXEC                                                             
                                                                                
           IF ERR-CRITICAL                                                      
              PERFORM 9990-END-ROUTINE                                          
           END-IF                                                               
           .                                                                    
       9500-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
      * ------------------------------------------------------------- *         
       9990-END-ROUTINE.                                                        
      * ------------------------------------------------------------ *          
                                                                                
           PERFORM 9000-TERMINATE                                               
                                                                                
           EXEC CICS RETURN                                                     
           END-EXEC                                                             
           .                                                                    
       9990-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
