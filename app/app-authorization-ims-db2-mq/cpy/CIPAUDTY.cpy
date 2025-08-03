      *****************************************************************         
      *   IMS SEGMENT - PENDING AUTHORIZATION DETAILS                           
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
           05  PA-AUTHORIZATION-KEY.
               10 PA-AUTH-DATE-9C           PIC S9(05) COMP-3.                  
               10 PA-AUTH-TIME-9C           PIC S9(09) COMP-3.                  
           05  PA-AUTH-ORIG-DATE            PIC  X(06).                         
           05  PA-AUTH-ORIG-TIME            PIC  X(06).                         
           05  PA-CARD-NUM                  PIC  X(16).                         
           05  PA-AUTH-TYPE                 PIC  X(04).                         
           05  PA-CARD-EXPIRY-DATE          PIC  X(04).                         
           05  PA-MESSAGE-TYPE              PIC  X(06).                         
           05  PA-MESSAGE-SOURCE            PIC  X(06).                         
           05  PA-AUTH-ID-CODE              PIC  X(06).                         
           05  PA-AUTH-RESP-CODE            PIC  X(02).                         
               88 PA-AUTH-APPROVED          VALUE '00'.                         
           05  PA-AUTH-RESP-REASON          PIC  X(04).                         
           05  PA-PROCESSING-CODE           PIC  9(06).                         
           05  PA-TRANSACTION-AMT           PIC S9(10)V99 COMP-3.               
           05  PA-APPROVED-AMT              PIC S9(10)V99 COMP-3.               
           05  PA-MERCHANT-CATAGORY-CODE    PIC  X(04).                         
           05  PA-ACQR-COUNTRY-CODE         PIC  X(03).                         
           05  PA-POS-ENTRY-MODE            PIC  9(02).                         
           05  PA-MERCHANT-ID               PIC  X(15).                         
           05  PA-MERCHANT-NAME             PIC  X(22).                         
           05  PA-MERCHANT-CITY             PIC  X(13).                         
           05  PA-MERCHANT-STATE            PIC  X(02).                         
           05  PA-MERCHANT-ZIP              PIC  X(09).                         
           05  PA-TRANSACTION-ID            PIC  X(15).                         
           05  PA-MATCH-STATUS              PIC  X(01).                         
               88 PA-MATCH-PENDING          VALUE 'P'.                          
               88 PA-MATCH-AUTH-DECLINED    VALUE 'D'.                          
               88 PA-MATCH-PENDING-EXPIRED  VALUE 'E'.                          
               88 PA-MATCHED-WITH-TRAN      VALUE 'M'.                          
           05  PA-AUTH-FRAUD                PIC  X(01).                         
               88 PA-FRAUD-CONFIRMED        VALUE 'F'.                          
               88 PA-FRAUD-REMOVED          VALUE 'R'.                          
           05  PA-FRAUD-RPT-DATE            PIC  X(08).                         
           05  FILLER                       PIC  X(17).                         
