000010******************************************************************00001000
000020* Copyright Amazon.com, Inc. or its affiliates.                   00002000
000030* All Rights Reserved.                                            00003000
000040*                                                                 00004000
000050* Licensed under the Apache License, Version 2.0 (the "License"). 00005000
000060* You may not use this file except in compliance with the License.00006000
000070* You may obtain a copy of the License at                         00007000
000080*                                                                 00008000
000090*    http://www.apache.org/licenses/LICENSE-2.0                   00009000
000091*                                                                 00009100
000092* Unless required by applicable law or agreed to in writing,      00009200
000093* software distributed under the License is distributed on an     00009300
000094* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    00009400
000095* either express or implied. See the License for the specific     00009500
000096* language governing permissions and limitations under the License00009600
000100******************************************************************00010000
      ******************************************************************
      * DCLGEN TABLE(CARDDEMO.TRANSACTION_TYPE_CATEGORY)               *
      *        LIBRARY(SNJARAO.AWS.DCL(DCLTRCAT))                      *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(DCL-)                                             *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE CARDDEMO.TRANSACTION_TYPE_CATEGORY TABLE
           ( TRC_TYPE_CODE                  CHAR(2) NOT NULL,
             TRC_TYPE_CATEGORY              CHAR(4) NOT NULL,
             TRC_CAT_DATA                   VARCHAR(50) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE CARDDEMO.TRANSACTION_TYPE_CATEGORY *
      ******************************************************************
       01  DCLTRANSACTION-TYPE-CATEGORY.
      *    *************************************************************
      *                       TRC_TYPE_CODE
           10 DCL-TRC-TYPE-CODE    PIC X(2).
      *    *************************************************************
      *                       TRC_TYPE_CATEGORY
           10 DCL-TRC-TYPE-CATEGORY
              PIC X(4).
      *    *************************************************************
           10 DCL-TRC-CAT-DATA.
      *                       TRC_CAT_DATA LENGTH
              49 DCL-TRC-CAT-DATA-LEN
                 PIC S9(4) USAGE COMP.
      *                       TRC_CAT_DATA
              49 DCL-TRC-CAT-DATA-TEXT
                 PIC X(50).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
