      ******************************************************************        
      * DCLGEN TABLE(AWSTSSC.AUTHFRDS)                                 *        
      *        LIBRARY(XXXXXXX.DB2.DCLGEN(AUTHFRDS))                   *
      *        LANGUAGE(COBOL)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
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

           EXEC SQL DECLARE CARDDEMO.AUTHFRDS TABLE
           ( CARD_NUM                       CHAR(16) NOT NULL,                  
             AUTH_TS                        TIMESTAMP NOT NULL,                 
             AUTH_TYPE                      CHAR(4),                            
             CARD_EXPIRY_DATE               CHAR(4),                            
             MESSAGE_TYPE                   CHAR(6),                            
             MESSAGE_SOURCE                 CHAR(6),                            
             AUTH_ID_CODE                   CHAR(6),                            
             AUTH_RESP_CODE                 CHAR(2),                            
             AUTH_RESP_REASON               CHAR(4),                            
             PROCESSING_CODE                CHAR(6),                            
             TRANSACTION_AMT                DECIMAL(12, 2),                     
             APPROVED_AMT                   DECIMAL(12, 2),                     
             MERCHANT_CATAGORY_CODE         CHAR(4),                            
             ACQR_COUNTRY_CODE              CHAR(3),                            
             POS_ENTRY_MODE                 SMALLINT,                           
             MERCHANT_ID                    CHAR(15),                           
             MERCHANT_NAME                  VARCHAR(22),                        
             MERCHANT_CITY                  CHAR(13),                           
             MERCHANT_STATE                 CHAR(2),                            
             MERCHANT_ZIP                   CHAR(9),                            
             TRANSACTION_ID                 CHAR(15),                           
             MATCH_STATUS                   CHAR(1),                            
             AUTH_FRAUD                     CHAR(1),                            
             FRAUD_RPT_DATE                 DATE,                               
             ACCT_ID                        DECIMAL(11, 0),                     
             CUST_ID                        DECIMAL(9, 0)                       
           ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE AWSTSSC.AUTHFRDS                   *        
      ******************************************************************        
       01  DCLAUTHFRDS.                                                         
           10 CARD-NUM             PIC X(16).                                   
           10 AUTH-TS              PIC X(26).                                   
           10 AUTH-TYPE            PIC X(4).                                    
           10 CARD-EXPIRY-DATE     PIC X(4).                                    
           10 MESSAGE-TYPE         PIC X(6).                                    
           10 MESSAGE-SOURCE       PIC X(6).                                    
           10 AUTH-ID-CODE         PIC X(6).                                    
           10 AUTH-RESP-CODE       PIC X(2).                                    
           10 AUTH-RESP-REASON     PIC X(4).                                    
           10 PROCESSING-CODE      PIC X(6).                                    
           10 TRANSACTION-AMT      PIC S9(10)V9(2) USAGE COMP-3.                
           10 APPROVED-AMT         PIC S9(10)V9(2) USAGE COMP-3.                
           10 MERCHANT-CATAGORY-CODE                                            
              PIC X(4).                                                         
           10 ACQR-COUNTRY-CODE    PIC X(3).                                    
           10 POS-ENTRY-MODE       PIC S9(4) USAGE COMP.                        
           10 MERCHANT-ID          PIC X(15).                                   
           10 MERCHANT-NAME.                                                    
              49 MERCHANT-NAME-LEN                                              
                 PIC S9(4) USAGE COMP.                                          
              49 MERCHANT-NAME-TEXT                                             
                 PIC X(22).                                                     
           10 MERCHANT-CITY        PIC X(13).                                   
           10 MERCHANT-STATE       PIC X(2).                                    
           10 MERCHANT-ZIP         PIC X(9).                                    
           10 TRANSACTION-ID       PIC X(15).                                   
           10 MATCH-STATUS         PIC X(1).                                    
           10 AUTH-FRAUD           PIC X(1).                                    
           10 FRAUD-RPT-DATE       PIC X(10).                                   
           10 ACCT-ID              PIC S9(11)V USAGE COMP-3.                    
           10 CUST-ID              PIC S9(9)V USAGE COMP-3.                     
      ******************************************************************        
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 26      *        
      ******************************************************************        
