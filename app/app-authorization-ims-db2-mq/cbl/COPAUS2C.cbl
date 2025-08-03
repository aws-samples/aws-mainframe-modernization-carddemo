      ******************************************************************
      * Program     : COPAUS2C.CBL
      * Application : CardDemo - Authorization Module
      * Type        : CICS COBOL IMS DB2 Program
      * Function    : Mark Authorization Message Fraud
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
       PROGRAM-ID. COPAUS2C.                                                    
       AUTHOR.     AWS.                                                         
                                                                                
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
                                                                                
       DATA DIVISION.                                                           
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01 WS-VARIABLES.                                                         
         05 WS-PGMNAME                 PIC X(08) VALUE 'COPAUS2C'.              
         05 WS-LENGTH                  PIC S9(4) COMP VALUE ZERO.               
         05 WS-AUTH-TIME               PIC 9(09).                               
         05 WS-AUTH-TIME-AN REDEFINES WS-AUTH-TIME                              
                                       PIC X(09).                               
         05 WS-AUTH-TS.                                                         
            10 WS-AUTH-YY              PIC X(02).                               
            10 FILLER                  PIC X(01) VALUE '-'.                     
            10 WS-AUTH-MM              PIC X(02).                               
            10 FILLER                  PIC X(01) VALUE '-'.                     
            10 WS-AUTH-DD              PIC X(02).                               
            10 FILLER                  PIC X(01) VALUE ' '.                     
            10 WS-AUTH-HH              PIC X(02).                               
            10 FILLER                  PIC X(01) VALUE '.'.                     
            10 WS-AUTH-MI              PIC X(02).                               
            10 FILLER                  PIC X(01) VALUE '.'.                     
            10 WS-AUTH-SS              PIC X(02).                               
            10 WS-AUTH-SSS             PIC X(03).                               
            10 FILLER                  PIC X(03) VALUE '000'.                   
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.                     
            88 ERR-FLG-ON                        VALUE 'Y'.                     
            88 ERR-FLG-OF                        VALUE 'N'.                     
         05 WS-SQLCODE                 PIC +9(06).                              
         05 WS-SQLSTATE                PIC +9(09).                              
                                                                                
         05 WS-ABS-TIME                PIC S9(15) COMP-3 VALUE 0.               
         05 WS-CUR-DATE                PIC X(08) VALUE SPACES.                  
                                                                                
      *COPY DFHBMSCA.                                                           
      /****************************************************                     
      * SQL INCLUDE FOR SQLCA                             *                     
      *****************************************************                     
           EXEC SQL                                                             
                INCLUDE SQLCA                                                   
           END-EXEC.                                                            
           EXEC SQL                                                             
                INCLUDE AUTHFRDS                                                
           END-EXEC.                                                            
                                                                                
                                                                                
       LINKAGE SECTION.                                                         
       01  DFHCOMMAREA.                                                         
           02 WS-ACCT-ID                    PIC 9(11).                          
           02 WS-CUST-ID                    PIC 9(9).                           
           02 WS-FRAUD-AUTH-RECORD.                                             
              COPY CIPAUDTY.                                                    
           02 WS-FRAUD-STATUS-RECORD.                                           
              05 WS-FRD-ACTION              PIC X(01).                          
                 88 WS-REPORT-FRAUD         VALUE 'F'.                          
                 88 WS-REMOVE-FRAUD         VALUE 'R'.                          
              05 WS-FRD-UPDATE-STATUS       PIC X(01).                          
                 88 WS-FRD-UPDT-SUCCESS     VALUE 'S'.                          
                 88 WS-FRD-UPDT-FAILED      VALUE 'F'.                          
              05 WS-FRD-ACT-MSG             PIC X(50).                          
                                                                                
       PROCEDURE DIVISION.                                                      
       MAIN-PARA.                                                               
                                                                                
           EXEC CICS ASKTIME NOHANDLE                                           
              ABSTIME(WS-ABS-TIME)                                              
              NOHANDLE                                                          
           END-EXEC                                                             
           EXEC CICS FORMATTIME                                                 
             ABSTIME(WS-ABS-TIME)                                               
             MMDDYY(WS-CUR-DATE)                                                
             DATESEP                                                            
             NOHANDLE                                                           
           END-EXEC                                                             
           MOVE WS-CUR-DATE       TO PA-FRAUD-RPT-DATE                          
                                                                                
           MOVE PA-AUTH-ORIG-DATE(1:2) TO WS-AUTH-YY                            
           MOVE PA-AUTH-ORIG-DATE(3:2) TO WS-AUTH-MM                            
           MOVE PA-AUTH-ORIG-DATE(5:2) TO WS-AUTH-DD                            
                                                                                
           COMPUTE WS-AUTH-TIME = 999999999 - PA-AUTH-TIME-9C                   
           MOVE WS-AUTH-TIME-AN(1:2) TO WS-AUTH-HH                              
           MOVE WS-AUTH-TIME-AN(3:2) TO WS-AUTH-MI                              
           MOVE WS-AUTH-TIME-AN(5:2) TO WS-AUTH-SS                              
           MOVE WS-AUTH-TIME-AN(7:3) TO WS-AUTH-SSS                             
                                                                                
           MOVE PA-CARD-NUM          TO CARD-NUM                                
           MOVE WS-AUTH-TS           TO AUTH-TS                                 
           MOVE PA-AUTH-TYPE         TO AUTH-TYPE                               
           MOVE PA-CARD-EXPIRY-DATE  TO CARD-EXPIRY-DATE                        
           MOVE PA-MESSAGE-TYPE      TO MESSAGE-TYPE                            
           MOVE PA-MESSAGE-SOURCE    TO MESSAGE-SOURCE                          
           MOVE PA-AUTH-ID-CODE      TO AUTH-ID-CODE                            
           MOVE PA-AUTH-RESP-CODE    TO AUTH-RESP-CODE                          
           MOVE PA-AUTH-RESP-REASON  TO AUTH-RESP-REASON                        
           MOVE PA-PROCESSING-CODE   TO PROCESSING-CODE                         
           MOVE PA-TRANSACTION-AMT   TO TRANSACTION-AMT                         
           MOVE PA-APPROVED-AMT      TO APPROVED-AMT                            
           MOVE PA-MERCHANT-CATAGORY-CODE                                       
                                     TO MERCHANT-CATAGORY-CODE                  
           MOVE PA-ACQR-COUNTRY-CODE TO ACQR-COUNTRY-CODE                       
           MOVE PA-POS-ENTRY-MODE    TO POS-ENTRY-MODE                          
           MOVE PA-MERCHANT-ID       TO MERCHANT-ID                             
           MOVE LENGTH OF PA-MERCHANT-NAME TO MERCHANT-NAME-LEN                 
           MOVE PA-MERCHANT-NAME     TO MERCHANT-NAME-TEXT                      
           MOVE PA-MERCHANT-CITY     TO MERCHANT-CITY                           
           MOVE PA-MERCHANT-STATE    TO MERCHANT-STATE                          
           MOVE PA-MERCHANT-ZIP      TO MERCHANT-ZIP                            
           MOVE PA-TRANSACTION-ID    TO TRANSACTION-ID                          
           MOVE PA-MATCH-STATUS      TO MATCH-STATUS                            
           MOVE WS-FRD-ACTION        TO AUTH-FRAUD                              
           MOVE WS-ACCT-ID           TO ACCT-ID                                 
           MOVE WS-CUST-ID           TO CUST-ID                                 
                                                                                
           EXEC SQL                                                             
                INSERT INTO CARDDEMO.AUTHFRDS
                      (CARD_NUM                                                 
                      ,AUTH_TS                                                  
                      ,AUTH_TYPE                                                
                      ,CARD_EXPIRY_DATE                                         
                      ,MESSAGE_TYPE                                             
                      ,MESSAGE_SOURCE                                           
                      ,AUTH_ID_CODE                                             
                      ,AUTH_RESP_CODE                                           
                      ,AUTH_RESP_REASON                                         
                      ,PROCESSING_CODE                                          
                      ,TRANSACTION_AMT                                          
                      ,APPROVED_AMT                                             
                      ,MERCHANT_CATAGORY_CODE                                   
                      ,ACQR_COUNTRY_CODE                                        
                      ,POS_ENTRY_MODE                                           
                      ,MERCHANT_ID                                              
                      ,MERCHANT_NAME                                            
                      ,MERCHANT_CITY                                            
                      ,MERCHANT_STATE                                           
                      ,MERCHANT_ZIP                                             
                      ,TRANSACTION_ID                                           
                      ,MATCH_STATUS                                             
                      ,AUTH_FRAUD                                               
                      ,FRAUD_RPT_DATE                                           
                      ,ACCT_ID                                                  
                      ,CUST_ID)                                                 
                  VALUES                                                        
                    ( :CARD-NUM                                                 
                     ,TIMESTAMP_FORMAT (:AUTH-TS,                               
                                        'YY-MM-DD HH24.MI.SSNNNNNN')            
                     ,:AUTH-TYPE                                                
                     ,:CARD-EXPIRY-DATE                                         
                     ,:MESSAGE-TYPE                                             
                     ,:MESSAGE-SOURCE                                           
                     ,:AUTH-ID-CODE                                             
                     ,:AUTH-RESP-CODE                                           
                     ,:AUTH-RESP-REASON                                         
                     ,:PROCESSING-CODE                                          
                     ,:TRANSACTION-AMT                                          
                     ,:APPROVED-AMT                                             
                     ,:MERCHANT-CATAGORY-CODE                                   
                     ,:ACQR-COUNTRY-CODE                                        
                     ,:POS-ENTRY-MODE                                           
                     ,:MERCHANT-ID                                              
                     ,:MERCHANT-NAME                                            
                     ,:MERCHANT-CITY                                            
                     ,:MERCHANT-STATE                                           
                     ,:MERCHANT-ZIP                                             
                     ,:TRANSACTION-ID                                           
                     ,:MATCH-STATUS                                             
                     ,:AUTH-FRAUD                                               
                     ,CURRENT DATE                                              
                     ,:ACCT-ID                                                  
                     ,:CUST-ID                                                  
                    )                                                           
           END-EXEC                                                             
           IF SQLCODE = ZERO                                                    
              SET WS-FRD-UPDT-SUCCESS TO TRUE                                   
              MOVE 'ADD SUCCESS'      TO WS-FRD-ACT-MSG                         
           ELSE                                                                 
              IF SQLCODE = -803                                                 
                 PERFORM FRAUD-UPDATE                                           
              ELSE                                                              
                 SET WS-FRD-UPDT-FAILED  TO TRUE                                
                                                                                
                 MOVE SQLCODE            TO WS-SQLCODE                          
                 MOVE SQLSTATE           TO WS-SQLSTATE                         
                                                                                
                 STRING ' SYSTEM ERROR DB2: CODE:' WS-SQLCODE                   
                        ', STATE: ' WS-SQLSTATE   DELIMITED BY SIZE             
                 INTO WS-FRD-ACT-MSG                                            
                 END-STRING                                                     
              END-IF                                                            
           END-IF                                                               
                                                                                
           EXEC CICS RETURN                                                     
           END-EXEC                                                             
           .                                                                    
       FRAUD-UPDATE.                                                            
           EXEC SQL                                                             
                UPDATE CARDDEMO.AUTHFRDS
                   SET   AUTH_FRAUD     = :AUTH-FRAUD,                          
                         FRAUD_RPT_DATE = CURRENT DATE                          
                   WHERE CARD_NUM = :CARD-NUM                                   
                     AND AUTH_TS  = TIMESTAMP_FORMAT (:AUTH-TS,                 
                                           'YY-MM-DD HH24.MI.SSNNNNNN')         
           END-EXEC                                                             
           IF SQLCODE = ZERO                                                    
              SET WS-FRD-UPDT-SUCCESS TO TRUE                                   
              MOVE 'UPDT SUCCESS'     TO WS-FRD-ACT-MSG                         
           ELSE                                                                 
              SET WS-FRD-UPDT-FAILED  TO TRUE                                   
                                                                                
              MOVE SQLCODE            TO WS-SQLCODE                             
              MOVE SQLSTATE           TO WS-SQLSTATE                            
                                                                                
              STRING ' UPDT ERROR DB2: CODE:' WS-SQLCODE                        
                     ', STATE: ' WS-SQLSTATE   DELIMITED BY SIZE                
              INTO WS-FRD-ACT-MSG                                               
              END-STRING                                                        
           END-IF                                                               
           .                                                                    
