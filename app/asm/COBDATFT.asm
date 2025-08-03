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
COBDATFT CSECT
         USING COBDATFT,R15
         STM   R14,R12,12(R13)      * STANDARD ENTRY
         LA    R12,SAVE             * LOAD 'SAVE' ADDRESS IN R12
         ST    R13,SAVE+4           * STORE R13 DATA IN SAVE+4
         ST    R12,8(,R13)          * STORE R12 (SAVE ADD) IN SAVE+8
         DROP  R15
         LR    R13,R12              * LOAD R13(STACK POINTER) WITH R12
         LR    R12,R15              * LOAD REGISTER 12 WITH R15
         USING COBDATFT,R12
         L     R2,0(R1)
*
         USING COREC,R2
         CLI   COINTYPE,C'1'
         BE    VALIDIN1
         CLI   COINTYPE,C'2'
         BE    VALIDIN2
         B     GOTOERR
VALIDIN1 EQU *
         CLC   COINPDT+4,=C'-'
         BE    GOTOERR
         CLI   COOUTYPE,C'2'
         BE    GOTOERR
         MVC   COOUTDT(4),COINPDT
         MVI   COOUTDT+4,C'-'
         MVC   COOUTDT+5(2),COINPDT+4
         MVI   COOUTDT+7,C'-'
         MVC   COOUTDT+8(2),COINPDT+6
         B     EXITL
VALIDIN2 EQU *
*        CLC   COINPDT+4,C'-'
*        BNE   GOTOERR
         CLI   COOUTYPE,C'1'
         BE    GOTOERR
         MVC   COOUTDT(4),COINPDT
         MVC   COOUTDT+4(2),COINPDT+5
         MVC   COOUTDT+6(2),COINPDT+8
         B     EXITL
GOTOERR  EQU *
         MVC   COERMSG,=C'INVALID INPUT'
EXITL    EQU *
*
         L     R13,SAVE+4
         LM    R14,R12,12(R13)
         SR    R15,R15
         BR    R14
*
SAVE     DS 18F
*
         COPY COCDATFT
*
R0     EQU 0
R1     EQU 1
R2     EQU 2
R3     EQU 3
R4     EQU 4
R5     EQU 5
R6     EQU 6
R7     EQU 7
R8     EQU 8
R9     EQU 9
R10    EQU 10
R11    EQU 11
R12    EQU 12
R13    EQU 13
R14    EQU 14
R15    EQU 15
         END
