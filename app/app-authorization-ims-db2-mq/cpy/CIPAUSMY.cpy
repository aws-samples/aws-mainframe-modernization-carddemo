      *****************************************************************         
      *   IMS SEGMENT - PENDING AUTHORIZATION SUMMARY                           
      *****************************************************************         
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
           05  PA-ACCT-ID                   PIC S9(11) COMP-3.
           05  PA-CUST-ID                   PIC  9(09).                         
           05  PA-AUTH-STATUS               PIC  X(01).                         
           05  PA-ACCOUNT-STATUS            PIC  X(02) OCCURS 5 TIMES.          
           05  PA-CREDIT-LIMIT              PIC S9(09)V99 COMP-3.               
           05  PA-CASH-LIMIT                PIC S9(09)V99 COMP-3.               
           05  PA-CREDIT-BALANCE            PIC S9(09)V99 COMP-3.               
           05  PA-CASH-BALANCE              PIC S9(09)V99 COMP-3.               
           05  PA-APPROVED-AUTH-CNT         PIC S9(04) COMP.                    
           05  PA-DECLINED-AUTH-CNT         PIC S9(04) COMP.                    
           05  PA-APPROVED-AUTH-AMT         PIC S9(09)V99 COMP-3.               
           05  PA-DECLINED-AUTH-AMT         PIC S9(09)V99 COMP-3.               
           05  FILLER                       PIC X(34).                          
