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
MVSWAIT  START 0
         USING *,15
INIT     STM   14,12,12(13)            STORE REGISTERS IN COBOL PROG
         L     5,0(1)                  LOAD R5 WITH ADDR OF DELAY
         L     1,0(5)                  LOAD R1 WITH DELAT VALUE
         ST    1,BINLBL                LOAD BINLBL WITH DELAY VALUE
         ASMWAIT BINLBL                START INTERVAL CONTROL TIMER
         LM    14,12,12(13)            RESTORE COBOL REGS
         MVI   12(13),X'FF'            *ML purpose of this? - can we remove it?
         SR    15,15                   ZERO R15 BEFORE RETURN
         BR    14                      RETURN TO COBOL
BINLBL   DS    F                       TIMER EVENT CONTROL BLOCK
         LTORG
         END
