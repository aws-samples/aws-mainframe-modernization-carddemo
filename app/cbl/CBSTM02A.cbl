       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    CBSTM02A.                                                 
       AUTHOR.        AWS.                                                      
      ******************************************************************
      * Program     : CBSTM02A.CBL                                      
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
           SELECT STMT-FILE ASSIGN TO STMTFILE.                                 
      *                                                                         
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
       FD  STMT-FILE.                                                           
       01  FD-STMTFILE-REC         PIC X(80).                                   
                                                                                
       WORKING-STORAGE SECTION.                                                 
                                                                                
       COPY COSTM01.                                                            
                                                                                
       COPY CVACT03Y.                                                           
                                                                                
       COPY CUSTREC.                                                            
                                                                                
       COPY CVACT01Y.                                                           
                                                                                
       01  COMP-VARIABLES          COMP.                                        
           05  CR-CNT              PIC S9(4) VALUE 0.                           
           05  TR-CNT              PIC S9(4) VALUE 0.                           
           05  CR-JMP              PIC S9(4) VALUE 0.                           
           05  TR-JMP              PIC S9(4) VALUE 0.                           
       01  COMP3-VARIABLES         COMP-3.                                      
           05  WS-TOTAL-AMT        PIC S9(9)V99 VALUE 0.                        
       01  MISC-VARIABLES.                                                      
           05  WS-FL-DD            PIC X(8) VALUE 'TRNXFILE'.                   
           05  WS-TRN-AMT          PIC S9(9)V99 VALUE 0.                        
           05  WS-SAVE-CARD VALUE SPACES PIC X(16).                             
           05  END-OF-FILE         PIC X(01) VALUE 'N'.                         
       01  WS-M02B-AREA.                                                        
           05  WS-M02B-DD          PIC X(08).                                   
           05  WS-M02B-OPER        PIC X(01).                                   
             88  M02B-OPEN       VALUE 'O'.                                     
             88  M02B-CLOSE      VALUE 'C'.                                     
             88  M02B-READ       VALUE 'R'.                                     
             88  M02B-READ-K     VALUE 'K'.                                     
             88  M02B-WRITE      VALUE 'W'.                                     
             88  M02B-REWRITE    VALUE 'Z'.                                     
           05  WS-M02B-RC          PIC X(02).                                   
           05  WS-M02B-KEY         PIC X(25).                                   
           05  WS-M02B-KEY-LN      PIC S9(4).                                   
           05  WS-M02B-FLDT        PIC X(1000).                                 
                                                                                
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
       01  ALIGN-PSA        PIC 9(16) BINARY.                                   
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
           05  TIOT-SEG.                                                        
               10  TIO-LEN  PIC X(01).                                          
               10  FILLER   PIC X(03).                                          
               10  TIOCDDNM PIC X(08).                                          
               10  FILLER   PIC X(05).                                          
               10  UCB-ADDR PIC X(03).                                          
                   88 NULL-UCB    VALUES LOW-VALUES.                            
           05  FILLER       PIC X(04).                                          
             88  END-OF-TIOT      VALUE LOW-VALUES.                             
      *****************************************************************         
       PROCEDURE DIVISION.                                                      
      *****************************************************************         
      *    Check Unit Control blocks                                  *         
      *****************************************************************         
           SET ADDRESS OF PSA-BLOCK   TO PSAPTR.                                
           SET ADDRESS OF TCB-BLOCK   TO TCB-POINT.                             
           SET ADDRESS OF TIOT-BLOCK  TO TIOT-POINT.                            
           SET TIOT-INDEX             TO TIOT-POINT.                            
           DISPLAY 'Running JCL : ' TIOTNJOB ' Step ' TIOTJSTP.                 
                                                                                
           COMPUTE BUMP-TIOT = BUMP-TIOT + LENGTH OF TIOT-BLOCK.                
           SET ADDRESS OF TIOT-ENTRY  TO TIOT-INDEX.                            
                                                                                
           DISPLAY 'DD Names from TIOT: '.                                      
           PERFORM UNTIL END-OF-TIOT                                            
                      OR TIO-LEN = LOW-VALUES                                   
               IF NOT NULL-UCB                                                  
                   DISPLAY ': ' TIOCDDNM ' -- valid UCB'                        
               ELSE                                                             
                   DISPLAY ': ' TIOCDDNM ' --  null UCB'                        
               END-IF                                                           
               COMPUTE BUMP-TIOT = BUMP-TIOT + LENGTH OF TIOT-SEG               
               SET ADDRESS OF TIOT-ENTRY TO TIOT-INDEX                          
           END-PERFORM.                                                         
                                                                                
           IF NOT NULL-UCB                                                      
               DISPLAY ': ' TIOCDDNM ' -- valid UCB'                            
           ELSE                                                                 
               DISPLAY ': ' TIOCDDNM ' -- null  UCB'                            
           END-IF.                                                              
                                                                                
           OPEN OUTPUT STMT-FILE.                                               
           INITIALIZE WS-TRNX-TABLE WS-TRN-TBL-CNTR.                            
                                                                                
       0000-START.                                                              
                                                                                
           EVALUATE WS-FL-DD                                                    
             WHEN 'TRNXFILE'                                                    
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
             WHEN 'READTRNX'                                                    
               GO TO 8500-READTRNX-READ                                         
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
                                                                                
           CLOSE STMT-FILE.                                                     
                                                                                
       9999-GOBACK.                                                             
           GOBACK.                                                              
                                                                                
      *---------------------------------------------------------------*         
       1000-XREFFILE-GET-NEXT.                                                  
                                                                                
           MOVE 'XREFFILE' TO WS-M02B-DD.                                       
           SET M02B-READ TO TRUE.                                               
           MOVE ZERO TO WS-M02B-RC.                                             
           MOVE SPACES TO WS-M02B-FLDT.                                         
           CALL 'CBSTM02B' USING WS-M02B-AREA.                                  
                                                                                
           EVALUATE WS-M02B-RC                                                  
             WHEN '00'                                                          
               CONTINUE                                                         
             WHEN '10'                                                          
               MOVE 'Y' TO END-OF-FILE                                          
             WHEN OTHER                                                         
               DISPLAY 'ERROR READING XREFFILE'                                 
               DISPLAY 'RETURN CODE: ' WS-M02B-RC                               
               PERFORM 9999-ABEND-PROGRAM                                       
           END-EVALUATE.                                                        
                                                                                
           MOVE WS-M02B-FLDT TO CARD-XREF-RECORD.                               
                                                                                
           EXIT.                                                                
                                                                                
       2000-CUSTFILE-GET.                                                       
                                                                                
           MOVE 'CUSTFILE' TO WS-M02B-DD.                                       
           SET M02B-READ-K TO TRUE.                                             
           MOVE XREF-CUST-ID TO WS-M02B-KEY.                                    
           MOVE ZERO TO WS-M02B-KEY-LN.                                         
           COMPUTE WS-M02B-KEY-LN = LENGTH OF XREF-CUST-ID.                     
           MOVE ZERO TO WS-M02B-RC.                                             
           MOVE SPACES TO WS-M02B-FLDT.                                         
           CALL 'CBSTM02B' USING WS-M02B-AREA.                                  
                                                                                
           EVALUATE WS-M02B-RC                                                  
             WHEN '00'                                                          
               CONTINUE                                                         
             WHEN OTHER                                                         
               DISPLAY 'ERROR READING CUSTFILE'                                 
               DISPLAY 'RETURN CODE: ' WS-M02B-RC                               
               PERFORM 9999-ABEND-PROGRAM                                       
           END-EVALUATE.                                                        
                                                                                
           MOVE WS-M02B-FLDT TO CUSTOMER-RECORD.                                
                                                                                
           EXIT.                                                                
                                                                                
       3000-ACCTFILE-GET.                                                       
                                                                                
           MOVE 'ACCTFILE' TO WS-M02B-DD.                                       
           SET M02B-READ-K TO TRUE.                                             
           MOVE XREF-ACCT-ID TO WS-M02B-KEY.                                    
           MOVE ZERO TO WS-M02B-KEY-LN.                                         
           COMPUTE WS-M02B-KEY-LN = LENGTH OF XREF-ACCT-ID.                     
           MOVE ZERO TO WS-M02B-RC.                                             
           MOVE SPACES TO WS-M02B-FLDT.                                         
           CALL 'CBSTM02B' USING WS-M02B-AREA.                                  
                                                                                
           EVALUATE WS-M02B-RC                                                  
             WHEN '00'                                                          
               CONTINUE                                                         
             WHEN OTHER                                                         
               DISPLAY 'ERROR READING ACCTFILE'                                 
               DISPLAY 'RETURN CODE: ' WS-M02B-RC                               
               PERFORM 9999-ABEND-PROGRAM                                       
           END-EVALUATE.                                                        
                                                                                
           MOVE WS-M02B-FLDT TO ACCOUNT-RECORD.                                 
                                                                                
           EXIT.                                                                
                                                                                
       4000-TRNXFILE-GET.                                                       
           PERFORM VARYING CR-JMP FROM 1 BY 1                                   
             UNTIL CR-JMP > CR-CNT                                              
             OR (WS-CARD-NUM (CR-JMP) > XREF-CARD-NUM)                          
               IF XREF-CARD-NUM = WS-CARD-NUM (CR-JMP)                          
                   MOVE WS-CARD-NUM (CR-JMP) TO TRNX-CARD-NUM                   
                   PERFORM VARYING TR-JMP FROM 1 BY 1                           
                     UNTIL (TR-JMP > WS-TRCT (CR-JMP))                          
                       MOVE WS-TRAN-NUM (CR-JMP, TR-JMP)                        
                         TO TRNX-ID                                             
                       MOVE WS-TRAN-REST (CR-JMP, TR-JMP)                       
                         TO TRNX-REST                                           
                       PERFORM 6000-WRITE-TRANS                                 
                       ADD TRNX-AMT TO WS-TOTAL-AMT                             
                   END-PERFORM                                                  
               END-IF                                                           
           END-PERFORM.                                                         
           MOVE WS-TOTAL-AMT TO WS-TRN-AMT.                                     
           MOVE WS-TRN-AMT TO ST-TOTAL-TRAMT.                                   
           WRITE FD-STMTFILE-REC FROM ST-LINE12.                                
           WRITE FD-STMTFILE-REC FROM ST-LINE14A.                               
           WRITE FD-STMTFILE-REC FROM ST-LINE15.                                
           EXIT.                                                                
      *---------------------------------------------------------------*         
       5000-CREATE-STATEMENT.                                                   
           INITIALIZE STATEMENT-LINES.                                          
           WRITE FD-STMTFILE-REC FROM ST-LINE0.                                 
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
                                                                                
           WRITE FD-STMTFILE-REC FROM ST-LINE1.                                 
           WRITE FD-STMTFILE-REC FROM ST-LINE2.                                 
           WRITE FD-STMTFILE-REC FROM ST-LINE3.                                 
           WRITE FD-STMTFILE-REC FROM ST-LINE4.                                 
           WRITE FD-STMTFILE-REC FROM ST-LINE5.                                 
           WRITE FD-STMTFILE-REC FROM ST-LINE6.                                 
           WRITE FD-STMTFILE-REC FROM ST-LINE5.                                 
           WRITE FD-STMTFILE-REC FROM ST-LINE7.                                 
           WRITE FD-STMTFILE-REC FROM ST-LINE8.                                 
           WRITE FD-STMTFILE-REC FROM ST-LINE9.                                 
           WRITE FD-STMTFILE-REC FROM ST-LINE10.                                
           WRITE FD-STMTFILE-REC FROM ST-LINE11.                                
           WRITE FD-STMTFILE-REC FROM ST-LINE12.                                
           WRITE FD-STMTFILE-REC FROM ST-LINE13.                                
           WRITE FD-STMTFILE-REC FROM ST-LINE12.                                
                                                                                
           EXIT.                                                                
                                                                                
      *---------------------------------------------------------------*         
       6000-WRITE-TRANS.                                                        
           MOVE TRNX-ID TO ST-TRANID.                                           
           MOVE TRNX-DESC TO ST-TRANDT.                                         
           MOVE TRNX-AMT TO ST-TRANAMT.                                         
           WRITE FD-STMTFILE-REC FROM ST-LINE14.                                
                                                                                
           EXIT.                                                                
                                                                                
      *---------------------------------------------------------------*         
       8100-FILE-OPEN.                                                          
           GO TO 8100-TRNXFILE-OPEN                                             
           .                                                                    
                                                                                
       8100-TRNXFILE-OPEN.                                                      
           MOVE 'TRNXFILE' TO WS-M02B-DD.                                       
           SET M02B-OPEN TO TRUE.                                               
           MOVE ZERO TO WS-M02B-RC.                                             
           CALL 'CBSTM02B' USING WS-M02B-AREA.                                  
                                                                                
           IF WS-M02B-RC = '00' OR '04'                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING TRNXFILE'                                 
               DISPLAY 'RETURN CODE: ' WS-M02B-RC                               
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF.                                                              
                                                                                
           SET M02B-READ TO TRUE.                                               
           MOVE SPACES TO WS-M02B-FLDT.                                         
           CALL 'CBSTM02B' USING WS-M02B-AREA.                                  
                                                                                
           IF WS-M02B-RC = '00' OR '04'                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR READING TRNXFILE'                                 
               DISPLAY 'RETURN CODE: ' WS-M02B-RC                               
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF.                                                              
                                                                                
           MOVE WS-M02B-FLDT TO TRNX-RECORD.                                    
           MOVE TRNX-CARD-NUM TO WS-SAVE-CARD.                                  
           MOVE 1 TO CR-CNT.                                                    
           MOVE 0 TO TR-CNT.                                                    
           MOVE 'READTRNX' TO WS-FL-DD.                                         
           GO TO 0000-START.                                                    
           EXIT.                                                                
                                                                                
      *---------------------------------------------------------------*         
       8200-XREFFILE-OPEN.                                                      
           MOVE 'XREFFILE' TO WS-M02B-DD.                                       
           SET M02B-OPEN TO TRUE.                                               
           MOVE ZERO TO WS-M02B-RC.                                             
           CALL 'CBSTM02B' USING WS-M02B-AREA.                                  
                                                                                
           IF WS-M02B-RC = '00' OR '04'                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING XREFFILE'                                 
               DISPLAY 'RETURN CODE: ' WS-M02B-RC                               
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF.                                                              
                                                                                
           MOVE 'CUSTFILE' TO WS-FL-DD.                                         
           GO TO 0000-START.                                                    
           EXIT.                                                                
      *---------------------------------------------------------------*         
       8300-CUSTFILE-OPEN.                                                      
           MOVE 'CUSTFILE' TO WS-M02B-DD.                                       
           SET M02B-OPEN TO TRUE.                                               
           MOVE ZERO TO WS-M02B-RC.                                             
           CALL 'CBSTM02B' USING WS-M02B-AREA.                                  
                                                                                
           IF WS-M02B-RC = '00' OR '04'                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING CUSTFILE'                                 
               DISPLAY 'RETURN CODE: ' WS-M02B-RC                               
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF.                                                              
                                                                                
           MOVE 'ACCTFILE' TO WS-FL-DD.                                         
           GO TO 0000-START.                                                    
           EXIT.                                                                
      *---------------------------------------------------------------*         
       8400-ACCTFILE-OPEN.                                                      
           MOVE 'ACCTFILE' TO WS-M02B-DD.                                       
           SET M02B-OPEN TO TRUE.                                               
           MOVE ZERO TO WS-M02B-RC.                                             
           CALL 'CBSTM02B' USING WS-M02B-AREA.                                  
                                                                                
           IF WS-M02B-RC = '00' OR '04'                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING ACCTFILE'                                 
               DISPLAY 'RETURN CODE: ' WS-M02B-RC                               
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF.                                                              
                                                                                
           GO TO 1000-MAINLINE.                                                 
           EXIT.                                                                
      *---------------------------------------------------------------*         
       8500-READTRNX-READ.                                                      
           IF WS-SAVE-CARD = TRNX-CARD-NUM                                      
               ADD 1 TO TR-CNT                                                  
           ELSE                                                                 
               MOVE TR-CNT TO WS-TRCT (CR-CNT)                                  
               ADD 1 TO CR-CNT                                                  
               MOVE 1 TO TR-CNT                                                 
           END-IF.                                                              
                                                                                
           MOVE TRNX-CARD-NUM TO WS-CARD-NUM (CR-CNT).                          
           MOVE TRNX-ID TO WS-TRAN-NUM (CR-CNT, TR-CNT).                        
           MOVE TRNX-REST TO WS-TRAN-REST (CR-CNT, TR-CNT).                     
           MOVE TRNX-CARD-NUM TO WS-SAVE-CARD.                                  
                                                                                
           MOVE 'TRNXFILE' TO WS-M02B-DD.                                       
           SET M02B-READ TO TRUE.                                               
           MOVE SPACES TO WS-M02B-FLDT.                                         
           CALL 'CBSTM02B' USING WS-M02B-AREA.                                  
                                                                                
           EVALUATE WS-M02B-RC                                                  
             WHEN '00'                                                          
               MOVE WS-M02B-FLDT TO TRNX-RECORD                                 
               GO TO 8500-READTRNX-READ                                         
             WHEN '10'                                                          
               GO TO 8599-EXIT                                                  
             WHEN OTHER                                                         
               DISPLAY 'ERROR READING TRNXFILE'                                 
               DISPLAY 'RETURN CODE: ' WS-M02B-RC                               
               PERFORM 9999-ABEND-PROGRAM                                       
           END-EVALUATE.                                                        
                                                                                
       8599-EXIT.                                                               
           MOVE TR-CNT TO WS-TRCT (CR-CNT).                                     
           MOVE 'XREFFILE' TO WS-FL-DD.                                         
           GO TO 0000-START.                                                    
           EXIT.                                                                
                                                                                
      *---------------------------------------------------------------*         
       9100-TRNXFILE-CLOSE.                                                     
           MOVE 'TRNXFILE' TO WS-M02B-DD.                                       
           SET M02B-CLOSE TO TRUE.                                              
           MOVE ZERO TO WS-M02B-RC.                                             
           CALL 'CBSTM02B' USING WS-M02B-AREA.                                  
                                                                                
           IF WS-M02B-RC = '00' OR '04'                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR CLOSING TRNXFILE'                                 
               DISPLAY 'RETURN CODE: ' WS-M02B-RC                               
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF.                                                              
                                                                                
           EXIT.                                                                
                                                                                
      *---------------------------------------------------------------*         
       9200-XREFFILE-CLOSE.                                                     
           MOVE 'XREFFILE' TO WS-M02B-DD.                                       
           SET M02B-CLOSE TO TRUE.                                              
           MOVE ZERO TO WS-M02B-RC.                                             
           CALL 'CBSTM02B' USING WS-M02B-AREA.                                  
                                                                                
           IF WS-M02B-RC = '00' OR '04'                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR CLOSING XREFFILE'                                 
               DISPLAY 'RETURN CODE: ' WS-M02B-RC                               
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF.                                                              
                                                                                
           EXIT.                                                                
      *---------------------------------------------------------------*         
       9300-CUSTFILE-CLOSE.                                                     
           MOVE 'CUSTFILE' TO WS-M02B-DD.                                       
           SET M02B-CLOSE TO TRUE.                                              
           MOVE ZERO TO WS-M02B-RC.                                             
           CALL 'CBSTM02B' USING WS-M02B-AREA.                                  
                                                                                
           IF WS-M02B-RC = '00' OR '04'                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR CLOSING CUSTFILE'                                 
               DISPLAY 'RETURN CODE: ' WS-M02B-RC                               
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF.                                                              
                                                                                
           EXIT.                                                                
      *---------------------------------------------------------------*         
       9400-ACCTFILE-CLOSE.                                                     
           MOVE 'ACCTFILE' TO WS-M02B-DD.                                       
           SET M02B-CLOSE TO TRUE.                                              
           MOVE ZERO TO WS-M02B-RC.                                             
           CALL 'CBSTM02B' USING WS-M02B-AREA.                                  
                                                                                
           IF WS-M02B-RC = '00' OR '04'                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR CLOSING ACCTFILE'                                 
               DISPLAY 'RETURN CODE: ' WS-M02B-RC                               
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF.                                                              
                                                                                
           EXIT.                                                                
                                                                                
       9999-ABEND-PROGRAM.                                                      
           DISPLAY 'ABENDING PROGRAM'                                           
           CALL 'CEE3ABD'.                                                      
                                                                                
