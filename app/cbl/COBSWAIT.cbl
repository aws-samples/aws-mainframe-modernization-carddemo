      ******************************************************************
      * PROGRAM     : COBSWAIT.CBL
      * Application : CardDemo
      * Type        : BATCH COBOL Program
      * FUNCTION    : UTILITY PROGRAM TO WAIT (PARM IN CENTISECONDS)
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
       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. COBSWAIT.                                             0002000
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.                                            00030000
       DATA DIVISION.                                                   00030900

       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
       01 MVSWAIT-TIME                    PIC 9(8) COMP.
       01 PARM-VALUE                      PIC X(8).


       PROCEDURE DIVISION.                                              00040000

           ACCEPT PARM-VALUE      FROM SYSIN.
           MOVE  PARM-VALUE       TO MVSWAIT-TIME.
           CALL 'MVSWAIT'       USING MVSWAIT-TIME.

           STOP RUN.                                                    00060000

