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
       01  CODATECN-REC.
           05  CODATECN-IN-REC.
               10  CODATECN-TYPE             PIC X.
                   88  YYYYMMDD-IN           VALUE "1".
                   88  YYYY-MM-DD-IN         VALUE "2".
               10  CODATECN-INP-DATE         PIC X(20).
               10  CODATECN-1INP REDEFINES CODATECN-INP-DATE.
                   15  CODATECN-1YYYY    PIC XXXX.
                   15  CODATECN-1MM      PIC XX.
                   15  CODATECN-1DD      PIC XX.
                   15  CODATECN-1FIL     PIC X(12).
               10  CODATECN-2INP REDEFINES CODATECN-INP-DATE.
                   15  CODATECN-1O-YYYY  PIC XXXX.
                   15  CODATECN-1I-S1    PIC X.
                   15  CODATECN-1MM      PIC XX.
                   15  CODATECN-1I-S2    PIC X.
                   15  CODATECN-2YY      PIC XX.
                   15  CODATECN-2FIL     PIC X(10).
           05  CODATECN-OUT-REC.
               10  CODATECN-OUTTYPE          PIC X.
                   88  YYYY-MM-DD-OP         VALUE "1".
                   88  YYYYMMDD-OP           VALUE "2".
               10  CODATECN-0UT-DATE         PIC X(20).
               10  CODATECN-1OUT REDEFINES CODATECN-0UT-DATE.
                   15  CODATECN-1O-YYYY  PIC XXXX.
                   15  CODATECN-1O-S1    PIC X.
                   15  CODATECN-1O-MM    PIC XX.
                   15  CODATECN-1O-S2    PIC X.
                   15  CODATECN-1O-DD    PIC XX.
                   15  CODATECN-1OFIl    PIC X(10).
               10  CODATECN-2OUT REDEFINES CODATECN-0UT-DATE.
                   15  CODATECN-2O-YYYY  PIC XXXX.
                   15  CODATECN-2O-MM    PIC XX.
                   15  CODATECN-2O-DD    PIC XX.
                   15  CODATECN-2OFIl    PIC X(12).
           05  CODATECN-ERROR-MSG        PIC X(38).
