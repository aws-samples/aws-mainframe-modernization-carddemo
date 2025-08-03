      ******************************************************************
      * CardDemo - Transaction altered Layout for use in reporting
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
      *
       01  TRNX-RECORD.
           05  TRNX-KEY.
               10  TRNX-CARD-NUM                       PIC X(16).
               10  TRNX-ID                             PIC X(16).
           05  TRNX-REST.
               10  TRNX-TYPE-CD                        PIC X(02).
               10  TRNX-CAT-CD                         PIC 9(04).
               10  TRNX-SOURCE                         PIC X(10).
               10  TRNX-DESC                           PIC X(100).
               10  TRNX-AMT                            PIC S9(09)V99.
               10  TRNX-MERCHANT-ID                    PIC 9(09).
               10  TRNX-MERCHANT-NAME                  PIC X(50).
               10  TRNX-MERCHANT-CITY                  PIC X(50).
               10  TRNX-MERCHANT-ZIP                   PIC X(10).
               10  TRNX-ORIG-TS                        PIC X(26).
               10  TRNX-PROC-TS                        PIC X(26).
               10  FILLER                              PIC X(20).


