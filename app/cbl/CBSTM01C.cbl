       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    CBSTM01C.                                                 
       AUTHOR.        AWS.                                   
      ******************************************************************
      * Program     : CBSTM01C.CBL                                      
      * Application : CardDemo                                          
      * Type        : BATCH COBOL Program                                
      * Function    : Print Account Statements from Transaction data.   
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
      * This program is to create statement based on the data in 
      * transaction file. The following features are excercised
      * to help create excercise modernization tooling
      ******************************************************************
      *  1. Mainframe Control block addressing
      *  2. Alter and GO TO statements
      *  3. COMP and COMP-3 variables
      *  4. 2 dimensional array
      ******************************************************************
       ENVIRONMENT DIVISION.                                                    
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
           SELECT TRNX-FILE ASSIGN TO TRNXFILE                                  
                  ORGANIZATION IS INDEXED                                       
                  ACCESS MODE  IS SEQUENTIAL                                    
                  RECORD KEY   IS FD-TRNXS-ID                                   
                  FILE STATUS  IS TRNXFILE-STATUS.                              
                                                                                
           SELECT XREF-FILE ASSIGN TO   XREFFILE                                
                  ORGANIZATION IS INDEXED                                       
                  ACCESS MODE  IS SEQUENTIAL                                    
                  RECORD KEY   IS FD-XREF-CARD-NUM                              
                  FILE STATUS  IS XREFFILE-STATUS.                              
                                                                                
           SELECT CUST-FILE ASSIGN TO CUSTFILE                                  
                  ORGANIZATION IS INDEXED                                       
                  ACCESS MODE  IS RANDOM                                        
                  RECORD KEY   IS FD-CUST-ID                                    
                  FILE STATUS  IS CUSTFILE-STATUS.                              
                                                                                
           SELECT ACCOUNT-FILE ASSIGN TO ACCTFILE                               
                  ORGANIZATION IS INDEXED                                       
                  ACCESS MODE  IS RANDOM                                        
                  RECORD KEY   IS FD-ACCT-ID                                    
                  FILE STATUS  IS ACCTFILE-STATUS.                              
                                                                                
      *                                                                         
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
       FD  TRNX-FILE.                                                           
       01  FD-TRNXFILE-REC.                                                     
           05 FD-TRNXS-ID.                                                      
              10  FD-TRNX-CARD                  PIC X(16).                      
              10  FD-TRNX-ID                    PIC X(16).                      
           05 FD-ACCT-DATA                      PIC X(318).                     
                                                                                
       FD  XREF-FILE.                                                           
       01  FD-XREFFILE-REC.                                                     
           05 FD-XREF-CARD-NUM                  PIC X(16).                      
           05 FD-XREF-DATA                      PIC X(34).                      
                                                                                
       FD  CUST-FILE.                                                           
       01  FD-CUSTFILE-REC.                                                     
           05 FD-CUST-ID                        PIC X(09).                      
           05 FD-CUST-DATA                      PIC X(491).                     
                                                                                
       FD  ACCOUNT-FILE.                                                        
       01  FD-ACCTFILE-REC.                                                     
           05 FD-ACCT-ID                        PIC 9(11).                      
           05 FD-ACCT-DATA                      PIC X(289).                     
                                                                                
       WORKING-STORAGE SECTION.                                                 
                                                                                
      *****************************************************************         
       COPY CVSTM01Y.                                                           
       01  TRNXFILE-STATUS.                                                     
           05  TRNXFILE-STAT1      PIC X.                                       
           05  TRNXFILE-STAT2      PIC X.                                       
                                                                                
       COPY CVACT03Y.                                                           
       01  XREFFILE-STATUS.                                                     
           05  XREFFILE-STAT1      PIC X.                                       
           05  XREFFILE-STAT2      PIC X.                                       
                                                                                
       COPY CUSTREC.                                                            
       01  CUSTFILE-STATUS.                                                     
           05  CUSTFILE-STAT1      PIC X.                                       
           05  CUSTFILE-STAT2      PIC X.                                       
                                                                                
       COPY CVACT01Y.                                                           
       01  ACCTFILE-STATUS.                                                     
           05  ACCTFILE-STAT1      PIC X.                                       
           05  ACCTFILE-STAT2      PIC X.                                       
                                                                                
       01  COMP-VARIABLES          COMP.                                        
           05  CR-CNT              PIC S9(4) VALUE 0.                           
           05  TR-CNT              PIC S9(4) VALUE 0.                           
           05  CR-JMP              PIC S9(4) VALUE 0.                           
           05  TR-JMP              PIC S9(4) VALUE 0.                           
           05  FL-CNT              PIC S9(4) VALUE 0.                           
       01  COMP3-VARIABLES         COMP-3.                                      
           05  WS-TOTAL-AMT        PIC S9(9)V99 VALUE 0.                        
       01  MISC-VARIABLES.                                                      
           05  WS-FL-DD            PIC X(8) VALUE 'TRANFILE'.                   
           05  WS-1ST-RD           PIC X(1) VALUE 'Y'.                          
             88  1ST-READ-Y      VALUE 'Y'.                                     
             88  1ST-READ-N      VALUE 'N'.                                     
           05  WS-TRN-AMT          PIC S9(9)V99 VALUE 0.                        
           05  WS-SAVE-CARD VALUE SPACES PIC X(16).                             
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
                                                                                
       01  WS-VALIDATION-TRAILER.                                               
          05  WS-VALIDATION-FAIL-REASON      PIC 9(04).                         
          05  WS-VALIDATION-FAIL-REASON-DESC PIC X(76).                         
                                                                                
       01  END-OF-FILE             PIC X(01)    VALUE 'N'.                      
       01  ABCODE                  PIC S9(9) BINARY.                            
       01  TIMING                  PIC S9(9) BINARY.                            
       01  COBOL-TS.                                                            
           05 COB-YYYY                  PIC X(04).                              
           05 COB-MM                    PIC X(02).                              
           05 COB-DD                    PIC X(02).                              
           05 COB-HH                    PIC X(02).                              
           05 COB-MIN                   PIC X(02).                              
           05 COB-SS                    PIC X(02).                              
           05 COB-MIL                   PIC X(02).                              
           05 COB-REST                  PIC X(05).                              
       01  DB2-FORMAT-TS                PIC X(26).                              
       01  FILLER REDEFINES DB2-FORMAT-TS.                                      
           06 DB2-YYYY                  PIC X(004).                       
           06 DB2-STREEP-1              PIC X.                            
           06 DB2-MM                    PIC X(002).                       
           06 DB2-STREEP-2              PIC X.                            
           06 DB2-DD                    PIC X(002).                       
           06 DB2-STREEP-3              PIC X.                            
           06 DB2-HH                    PIC X(002).                       
           06 DB2-DOT-1                 PIC X.                            
           06 DB2-MIN                   PIC X(002).                       
           06 DB2-DOT-2                 PIC X.                            
           06 DB2-SS                    PIC X(002).                             
           06 DB2-DOT-3                 PIC X.                                  
           06 DB2-MIL                   PIC 9(002).                             
           06 DB2-REST                  PIC X(04).                              
                                                                                
       01  STATEMENT-LINES.                                                     
           05  ST-LINE0.                                                        
               10  FILLER  VALUE ALL '*'                PIC X(31).              
               10  FILLER  VALUE ALL 'START OF STATEMENT' PIC X(18).            
               10  FILLER  VALUE ALL '*'                PIC X(31).              
           05  ST-LINE1.                                                        
               10  ST-NAME                              PIC X(75).              
               10  FILLER  VALUE SPACES                 PIC X(05).              
           05  ST-LINE2.                                                        
               10  ST-ADD1                              PIC X(50).              
               10  FILLER  VALUE SPACES                 PIC X(30).              
           05  ST-LINE3.                                                        
               10  ST-ADD2                              PIC X(50).              
               10  FILLER  VALUE SPACES                 PIC X(30).              
           05  ST-LINE4.                                                        
               10  ST-ADD3                              PIC X(80).              
           05  ST-LINE5.                                                        
               10  FILLER  VALUE ALL '-'                PIC X(80).              
           05  ST-LINE6.                                                        
               10  FILLER  VALUE SPACES                 PIC X(33).              
               10  FILLER  VALUE 'Basic Details'        PIC X(14).              
               10  FILLER  VALUE SPACES                 PIC X(33).              
           05  ST-LINE7.                                                        
               10  FILLER  VALUE 'Account ID         :' PIC X(20).              
               10  ST-ACCT-ID                           PIC X(20).              
               10  FILLER  VALUE SPACES                 PIC X(40).              
           05  ST-LINE8.                                                        
               10  FILLER  VALUE 'Current Balance    :' PIC X(20).              
               10  ST-CURR-BAL                          PIC 9(9).99-.           
               10  FILLER  VALUE SPACES                 PIC X(07).              
               10  FILLER  VALUE SPACES                 PIC X(40).              
           05  ST-LINE9.                                                        
               10  FILLER  VALUE 'FICO Score         :' PIC X(20).              
               10  ST-FICO-SCORE                        PIC X(20).              
               10  FILLER  VALUE SPACES                 PIC X(40).              
           05  ST-LINE10.                                                       
               10  FILLER  VALUE ALL '-'                PIC X(80).              
           05  ST-LINE11.                                                       
               10  FILLER  VALUE SPACES                 PIC X(30).              
               10  FILLER  VALUE 'TRANSACTION SUMMARY ' PIC X(20).              
               10  FILLER  VALUE SPACES                 PIC X(30).              
           05  ST-LINE12.                                                       
               10  FILLER  VALUE ALL '-'                PIC X(80).              
           05  ST-LINE13.                                                       
               10  FILLER  VALUE 'Tran ID         '     PIC X(16).              
               10  FILLER  VALUE 'Tran Details    '     PIC X(51).              
               10  FILLER  VALUE '  Tran Amount'        PIC X(13).              
           05  ST-LINE14.                                                       
               10  ST-TRANID                            PIC X(16).              
               10  FILLER            VALUE ' '          PIC X(01).              
               10  ST-TRANDT                            PIC X(49).              
               10  FILLER            VALUE '$'          PIC X(01).              
               10  ST-TRANAMT                           PIC Z(9).99-.           
           05  ST-LINE14A.                                                      
               10  FILLER            VALUE 'Total EXP:' PIC X(10).              
               10  FILLER            VALUE SPACES       PIC X(56).              
               10  FILLER            VALUE '$'          PIC X(01).              
               10  ST-TOTAL-TRAMT                       PIC Z(9).99-.           
           05  ST-LINE15.                                                       
               10  FILLER  VALUE ALL '*'                PIC X(32).              
               10  FILLER  VALUE ALL 'END OF STATEMENT' PIC X(16).              
               10  FILLER  VALUE ALL '*'                PIC X(32).              
                                                                                
       01  WS-TRNX-TABLE.                                                       
           05  WS-CARD-TBL OCCURS 51 TIMES.                                     
               10  WS-CARD-NUM                          PIC X(16).              
               10  WS-TRAN-TBL OCCURS 10 TIMES.                                 
                   15  WS-TRAN-NUM                      PIC X(16).              
                   15  WS-TRAN-REST                     PIC X(318).             
       01  WS-TRN-TBL-CNTR.                                                     
           05  WS-TRN-TBL-CTR OCCURS 51 TIMES.                                  
               10  WS-TRCT               PIC S9(4) COMP.                        
                                                                       
       01  PSAPTR                  POINTER.                            
       01  BUMP-TIOT               PIC S9(08) BINARY VALUE ZERO.       
       01  TIOT-INDEX              REDEFINES BUMP-TIOT POINTER.   
       
       LINKAGE SECTION.                                                
       01  ALIGN-PSA               PIC 9(16) BINARY.                   
       01  PSA-BLOCK.                                                  
           05  FILLER       PIC X(536).                                
           05  TCB-POINT    POINTER.                                   
       01  TCB-BLOCK.                                                  
           05  FILLER       PIC X(12).                                 
           05  TIOT-POINT   POINTER.                                   
       01  TIOT-BLOCK.                                                 
           05  TIOTNJOB     PIC X(08).                                 
           05  TIOTJSTP     PIC X(08).                                 
           05  TIOTPSTP     PIC X(08).                                 
       01  TIOT-ENTRY.                                                 
           05  ONE-TIOT.                                               
               10  FILLER   PIC X(04).                                 
               10  TIOCDDNM PIC X(08).                                 
               10  FILLER   PIC X(05).                                 
               10  UCB-ADDR PIC X(03).                                 
                 88  NULL-UCB     VALUE LOW-VALUES.                    
           05  FILLER       PIC X(04).                                 
             88  END-OF-TIOT      VALUE LOW-VALUES.                    
                                                                        
      *****************************************************************         
       PROCEDURE DIVISION.                                                      
      *****************************************************************
      * GATHER SYSTEM LEVEL INFORMATION FROM MAINFRAME BLOCKS         *
      * 1. NAME OF THE JOB WHICH IS RUNNING THIS PROGRAM.             *
      * 2. JOB STEP WHERE THIS PROGRAM IS GETTING EXECUTED.           *
      * 3. DD NAMES OF ALL THE FILES GETTING USED IN THE STEP.        *
      *****************************************************************
           SET ADDRESS OF PSA-BLOCK   TO PSAPTR.                        
           SET ADDRESS OF TCB-BLOCK   TO TCB-POINT.                     
           SET ADDRESS OF TIOT-BLOCK  TO TIOT-POINT.                    
           SET TIOT-INDEX             TO TIOT-POINT.                  
           DISPLAY 'JOB NAME    : ' TIOTNJOB.                          
           DISPLAY 'STEP NAME   : ' TIOTJSTP.                          
      *    DISPLAY 'PSAPTR      : ' PSAPTR                             
      *    DISPLAY 'TCBBLK      : ' TCB-BLOCK                          
           COMPUTE BUMP-TIOT = BUMP-TIOT + LENGTH OF TIOT-BLOCK.       
           SET ADDRESS OF TIOT-ENTRY TO TIOT-INDEX.                    
                                                                       
           PERFORM UNTIL END-OF-TIOT                                   
      *        DISPLAY 'TIOT-ENTRY  : ' TIOT-ENTRY                     
               IF NOT NULL-UCB                                         
                   DISPLAY 'DD NAME     : ' TIOCDDNM ' HAD VALID UCB'  
               ELSE                                                    
                   DISPLAY 'DD NAME     : ' TIOCDDNM ' HAD NULL UCB'   
               END-IF                                                  
               COMPUTE BUMP-TIOT = BUMP-TIOT + LENGTH OF ONE-TIOT      
               SET ADDRESS OF TIOT-ENTRY TO TIOT-INDEX                 
           END-PERFORM.
           
      *    DISPLAY 'TIOT-ENTRY  : ' TIOT-ENTRY                         
           IF NOT NULL-UCB                                             
               DISPLAY 'DD NAME     : ' TIOCDDNM ' HAD VALID UCB'      
           ELSE                                                        
               DISPLAY 'DD NAME     : ' TIOCDDNM ' HAD NULL UCB'       
           END-IF.                                                     
                                                                        
           INITIALIZE WS-TRNX-TABLE WS-TRN-TBL-CNTR.                            
           MOVE 0 TO FL-CNT.                                                    
           SET 1ST-READ-Y TO TRUE.                                              
                                                                                
       0000-START.                                                              
           ADD 1 TO FL-CNT.
           EVALUATE WS-FL-DD                                                    
             WHEN 'TRANFILE'                                                    
               ALTER 8100-FILE-OPEN TO PROCEED TO 8100-TRNXFILE-OPEN
               GO TO 8100-FILE-OPEN
             WHEN 'XREFFILE'                                                    
               ALTER 8100-FILE-OPEN TO PROCEED TO 8200-XREFFILE-OPEN    
               GO TO 8100-FILE-OPEN
             WHEN 'CUSTFILE'                                                    
               ALTER 8100-FILE-OPEN TO PROCEED TO 8300-CUSTFILE-OPEN    
               GO TO 8100-FILE-OPEN
             WHEN 'ACCTFILE'                                                    
               ALTER 8100-FILE-OPEN TO PROCEED TO 8400-ACCTFILE-OPEN    
               GO TO 8100-FILE-OPEN
             WHEN 'READTRAN'                                                    
               GO TO 8500-READTRAN-READ                                         
             WHEN OTHER                                                         
               GO TO 9999-GOBACK.
                                                                              
       1000-MAINLINE.                                                           
           PERFORM UNTIL END-OF-FILE = 'Y'                                      
               IF  END-OF-FILE = 'N'                                            
                   PERFORM 1000-XREFFILE-GET-NEXT                               
                   IF  END-OF-FILE = 'N'                                        
                       PERFORM 2000-CUSTFILE-GET                                
                       PERFORM 3000-ACCTFILE-GET                                
                       PERFORM 5000-CREATE-STATEMENT                            
                       MOVE 1 TO CR-JMP                                         
                       MOVE ZERO TO WS-TOTAL-AMT                                
                       PERFORM 4000-TRNXFILE-GET                                
                   END-IF                                                       
               END-IF                                                           
           END-PERFORM.                                                         
                                                                                
           PERFORM 9100-TRNXFILE-CLOSE.   
           PERFORM 9200-XREFFILE-CLOSE.          
           PERFORM 9300-CUSTFILE-CLOSE.          
           PERFORM 9400-ACCTFILE-CLOSE.          
   







   
       9999-GOBACK.                                                     
           GOBACK.                                                      
                                                                        
      *---------------------------------------------------------------*         
       1000-XREFFILE-GET-NEXT.                                                  
           READ XREF-FILE INTO CARD-XREF-RECORD.                                
           IF  XREFFILE-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               IF  XREFFILE-STATUS = '10'                                       
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
                   DISPLAY 'ERROR READING XREFFILE FILE'                        
                   MOVE XREFFILE-STATUS TO IO-STATUS                            
                   PERFORM 9910-DISPLAY-IO-STATUS                               
                   PERFORM 9999-ABEND-PROGRAM                                   
               END-IF                                                           
           END-IF                                                               
           EXIT.                                                                
                                                                                
       2000-CUSTFILE-GET.                                                       
           MOVE XREF-CUST-ID TO FD-CUST-ID                                      
           READ CUST-FILE INTO CUSTOMER-RECORD                                  
              INVALID KEY                                                       
                MOVE 100 TO WS-VALIDATION-FAIL-REASON                           
                MOVE 'READ FAILED FOR CUST FILE'                                
                  TO WS-VALIDATION-FAIL-REASON-DESC                             
              NOT INVALID KEY                                                   
      *           DISPLAY 'ACCOUNT RECORD FOUND'                                
                  CONTINUE                                                      
           END-READ                                                             
      *    MOVE FD-CUSTFILE-REC TO CUSTOMER-RECORD.                             
           EXIT.                                                                
                                                                                
       3000-ACCTFILE-GET.                                                       
           MOVE XREF-ACCT-ID TO FD-ACCT-ID                                      
           READ ACCOUNT-FILE INTO ACCOUNT-RECORD                                
              INVALID KEY                                                       
                MOVE 100 TO WS-VALIDATION-FAIL-REASON                           
                MOVE 'READ FAILED FOR ACCT FILE'                                
                  TO WS-VALIDATION-FAIL-REASON-DESC                             
              NOT INVALID KEY                                                   
      *           DISPLAY 'ACCOUNT RECORD FOUND'                                
                  CONTINUE                                                      
           END-READ                                                             
      *    MOVE FD-ACCTFILE-REC TO ACCOUNT-RECORD.                              
           EXIT.                                                                
                                                                                
       4000-TRNXFILE-GET.                                                       
           PERFORM VARYING CR-JMP FROM 1 BY 1                                   
             UNTIL CR-JMP > CR-CNT                                              
             OR (WS-CARD-NUM (CR-JMP) > XREF-CARD-NUM)                          
               IF XREF-CARD-NUM = WS-CARD-NUM (CR-JMP)                          
      *            DISPLAY 'CR-JMP: ' CR-JMP ' ; '                              
      *              WS-CARD-NUM (CR-JMP) ' ; ' WS-TRCT (CR-JMP)                
                   MOVE WS-CARD-NUM (CR-JMP) TO TRNX-CARD-NUM                   
                   PERFORM VARYING TR-JMP FROM 1 BY 1                           
                     UNTIL (TR-JMP > WS-TRCT (CR-JMP))                          
      *                DISPLAY 'CR-JMP: ' CR-JMP ' ; '                          
      *                  WS-TRAN-REST (CR-JMP, TR-JMP)                          
      *                  ' ; ' WS-TRAN-NUM (CR-JMP, TR-JMP)                     
                       MOVE WS-TRAN-NUM (CR-JMP, TR-JMP)                        
                         TO TRNX-ID                                             
                       MOVE WS-TRAN-REST (CR-JMP, TR-JMP)                       
                         TO TRNX-REST                                           
                       PERFORM 6000-WRITE-TRANS                                 
                       ADD TRNX-AMT TO WS-TOTAL-AMT                             
                   END-PERFORM                                                  
      *        ELSE                                                             
      *            SUBTRACT 1 FROM CR-JMP                                       
               END-IF                                                           
           END-PERFORM.                                                         
      *    PERFORM UNTIL XREF-CARD-NUM NOT = TRNX-CARD-NUM                      
      *        IF XREF-CARD-NUM = TRNX-CARD-NUM                                 
      *            PERFORM 6000-WRITE-TRANS                                     
      *        END-IF                                                           
      *        READ TRNX-FILE INTO TRNX-RECORD                                  
      *        END-READ                                                         
      *        IF TRNXFILE-STATUS = '00' OR '04'                                
      *            CONTINUE                                                     
      *        ELSE                                                             
      *            MOVE LOW-VALUES TO TRNX-CARD-NUM                             
      *        END-IF                                                           
      *    END-PERFORM.                                                         
           MOVE WS-TOTAL-AMT TO WS-TRN-AMT.                                     
           MOVE WS-TRN-AMT TO ST-TOTAL-TRAMT.                                   
           DISPLAY ST-LINE12.                                                   
           DISPLAY ST-LINE14A.                                                  
           DISPLAY ST-LINE15.                                                   
           EXIT.                                                                
      *---------------------------------------------------------------*         
       5000-CREATE-STATEMENT.                                                   
           INITIALIZE STATEMENT-LINES.                                          
           DISPLAY ST-LINE0.                                                    
           STRING CUST-FIRST-NAME DELIMITED BY ' '                              
                  ' ' DELIMITED BY SIZE                                         
                  CUST-MIDDLE-NAME DELIMITED BY ' '                             
                  ' ' DELIMITED BY SIZE                                         
                  CUST-LAST-NAME DELIMITED BY ' '                               
                  ' ' DELIMITED BY SIZE                                         
                  INTO ST-NAME                                                  
           END-STRING.                                                          
           MOVE CUST-ADDR-LINE-1 TO ST-ADD1.                                    
           MOVE CUST-ADDR-LINE-2 TO ST-ADD2.                                    
           STRING CUST-ADDR-LINE-3 DELIMITED BY ' '                             
                  ' ' DELIMITED BY SIZE                                         
                  CUST-ADDR-STATE-CD DELIMITED BY ' '                           
                  ' ' DELIMITED BY SIZE                                         
                  CUST-ADDR-COUNTRY-CD DELIMITED BY ' '                         
                  ' ' DELIMITED BY SIZE                                         
                  CUST-ADDR-ZIP DELIMITED BY ' '                                
                  ' ' DELIMITED BY SIZE                                         
                  INTO ST-ADD3                                                  
           END-STRING.                                                          
                                                                                
           MOVE ACCT-ID TO ST-ACCT-ID.                                          
           MOVE ACCT-CURR-BAL TO ST-CURR-BAL.                                   
           MOVE CUST-FICO-CREDIT-SCORE TO ST-FICO-SCORE.                        
                                                                                
           DISPLAY ST-LINE1.                                                    
           DISPLAY ST-LINE2.                                                    
           DISPLAY ST-LINE3.                                                    
           DISPLAY ST-LINE4.                                                    
           DISPLAY ST-LINE5.                                                    
           DISPLAY ST-LINE6.                                                    
           DISPLAY ST-LINE5.                                                    
           DISPLAY ST-LINE7.                                                    
           DISPLAY ST-LINE8.                                                    
           DISPLAY ST-LINE9.                                                    
           DISPLAY ST-LINE10.                                                   
           DISPLAY ST-LINE11.                                                   
           DISPLAY ST-LINE12.                                                   
           DISPLAY ST-LINE13.                                                   
           DISPLAY ST-LINE12.                                                   
                                                                                
           EXIT.                                                                
                                                                                
      *---------------------------------------------------------------*         
       6000-WRITE-TRANS.                                                        
           MOVE TRNX-ID TO ST-TRANID.                                           
           MOVE TRNX-DESC TO ST-TRANDT.                                         
           MOVE TRNX-AMT TO ST-TRANAMT.                                         
           DISPLAY ST-LINE14.                                                   
                                                                                
           EXIT.                                                                
                                                                                
      *---------------------------------------------------------------*
       8100-FILE-OPEN.
           GO TO 8100-TRNXFILE-OPEN
           .
                                                                                
       8100-TRNXFILE-OPEN.                                                      
           MOVE 8 TO APPL-RESULT.                                               
           OPEN INPUT TRNX-FILE.                                                
           IF  TRNXFILE-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF.      
      
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING TRNXSACTION FILE'                         
               MOVE TRNXFILE-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF.                                                              
           READ TRNX-FILE INTO TRNX-RECORD.                                     
           MOVE TRNX-CARD-NUM TO WS-SAVE-CARD.                                  
           MOVE 1 TO CR-CNT.                                                    
           MOVE 0 TO TR-CNT.                                                    
           MOVE 'READTRAN' TO WS-FL-DD.                                         
           GO TO 0000-START.                                                    
           EXIT.                                                                
                                                                                
      *---------------------------------------------------------------*         
       8200-XREFFILE-OPEN.                                                      
           MOVE 8 TO APPL-RESULT.                                               
           OPEN INPUT XREF-FILE.                                                
           IF  XREFFILE-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF.    
        
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING CROSS REF FILE'                           
               MOVE XREFFILE-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF.                                                              
           MOVE 'CUSTFILE' TO WS-FL-DD.                                         
           GO TO 0000-START.                                                    
           EXIT.                                                                
      *---------------------------------------------------------------*         
       8300-CUSTFILE-OPEN.                                                      
           MOVE 8 TO APPL-RESULT.                                               
           OPEN INPUT CUST-FILE.                                                
           IF  CUSTFILE-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF.      
    
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING CUST FILE'                                
               MOVE CUSTFILE-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF.                                                              
           MOVE 'ACCTFILE' TO WS-FL-DD.                                         
           GO TO 0000-START.                                                    
           EXIT.                                                                
      *---------------------------------------------------------------*         
       8400-ACCTFILE-OPEN.                                                      
           MOVE 8 TO APPL-RESULT.                                               
           OPEN INPUT  ACCOUNT-FILE.                                            
           IF  ACCTFILE-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF.   
      
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING ACCOUNT MASTER FILE'                      
               MOVE ACCTFILE-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF.                                                              
           GO TO 1000-MAINLINE.                                                 
           EXIT.                                                                
      *---------------------------------------------------------------*         
       8500-READTRAN-READ.                                                      
           IF WS-SAVE-CARD = TRNX-CARD-NUM                                      
               ADD 1 TO TR-CNT                                                  
           ELSE                                                                 
               MOVE TR-CNT TO WS-TRCT (CR-CNT)                                  
               ADD 1 TO CR-CNT                                                  
               MOVE 1 TO TR-CNT                                                 
           END-IF.                                                              
      *    DISPLAY '8500- ' CR-CNT ' ; ' TR-CNT ' ; '                           
      *    TRNX-REST                                                            
                                                                                
           MOVE TRNX-CARD-NUM TO WS-CARD-NUM (CR-CNT).                          
           MOVE TRNX-ID TO WS-TRAN-NUM (CR-CNT, TR-CNT).                        
           MOVE TRNX-REST TO WS-TRAN-REST (CR-CNT, TR-CNT).                     
           MOVE TRNX-CARD-NUM TO WS-SAVE-CARD.                                  
                                                                                
           READ TRNX-FILE INTO TRNX-RECORD                                      
             AT END                                                             
               GO TO 8599-EXIT                                                  
             NOT AT END                                                         
               GO TO 8500-READTRAN-READ                                         
           END-READ.                                                            
                                                                                
       8599-EXIT.                                                               
           MOVE TR-CNT TO WS-TRCT (CR-CNT).                                     
           MOVE 'XREFFILE' TO WS-FL-DD.                                         
           GO TO 0000-START.                                                    
           EXIT.                                                                
                                                                                
      *---------------------------------------------------------------*         
       9100-TRNXFILE-CLOSE.                                                     
           MOVE 8 TO  APPL-RESULT.                                              
           CLOSE TRNX-FILE                                                      
           IF  TRNXFILE-STATUS = '00'                                           
               MOVE 0 TO  APPL-RESULT                                           
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF       
       
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR CLOSING TRNXSACTION FILE'                         
               MOVE TRNXFILE-STATUS  TO IO-STATUS                               
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
                                                                                
      *---------------------------------------------------------------*         
       9200-XREFFILE-CLOSE.                                                     
           MOVE 8 TO APPL-RESULT.                                               
           CLOSE XREF-FILE                                                      
           IF  XREFFILE-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF     
     
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR CLOSING CROSS REF FILE'                           
               MOVE XREFFILE-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       9300-CUSTFILE-CLOSE.                                                     
           MOVE 8 TO APPL-RESULT.                                               
           CLOSE CUST-FILE                                                      
           IF  CUSTFILE-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF 
         
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR CLOSING CUST FILE'                                
               MOVE XREFFILE-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       9400-ACCTFILE-CLOSE.                                                     
           MOVE 8 TO APPL-RESULT.                                               
           CLOSE ACCOUNT-FILE                                                   
           IF  ACCTFILE-STATUS  = '00'                                          
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF  
         
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR CLOSING ACCOUNT FILE'                             
               MOVE ACCTFILE-STATUS  TO IO-STATUS                               
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
                                                                                
       9999-ABEND-PROGRAM.                                                      
           DISPLAY 'ABENDING PROGRAM'                                           
           MOVE 0 TO TIMING                                                     
           MOVE 999 TO ABCODE                                                   
           CALL 'CEE3ABD'.                                                      
                                                                                
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
                                                                                
