//*********************************************************************
//*  THIS PROC IS USED TO COMPILE BATCH PGMS
//*********************************************************************
//* Copyright Amazon.com, Inc. or its affiliates.                   
//* All Rights Reserved.                                            
//*                                                                 
//* Licensed under the Apache License, Version 2.0 (the "License"). 
//* You may not use this file except in compliance with the License.
//* You may obtain a copy of the License at                         
//*                                                                 
//*    http://www.apache.org/licenses/LICENSE-2.0                   
//*                                                                 
//* Unless required by applicable law or agreed to in writing,      
//* software distributed under the License is distributed on an     
//* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    
//* either express or implied. See the License for the specific     
//* language governing permissions and limitations under the License
//*********************************************************************
//****  Sample Batch COBOL Compile PROC                          ******         
//****  Check with your Administrator for                        ******         
//****  JCL suitable to your environment                         ******
//*********************************************************************
//*
//BLDBAT PROC MEM=,
//            HLQ=,
//            COBLIB=IGY.SIGYCOMP.V63,
//            CPYBKS=&HLQ..CARDDEMO.CPY,
//            SOURCE=&HLQ..CARDDEMO.CBL,
//            LOADLIB=&HLQ..CARDDEMO.LOADLIB,
//            LISTING=&HLQ..CARDDEMO.LST
//*  ---------------------------
//*  COBOL PROGRAM COMPILE STEP
//*  ---------------------------
//*
//COMPILE EXEC PGM=IGYCRCTL,REGION=0M,
//   PARM=(APOST,LIST,MAP,NUMBER,NOSEQ)
//STEPLIB  DD DSN=&COBLIB,DISP=SHR
//SYSIN    DD DISP=SHR,DSN=&SOURCE(&MEM)
//SYSLIB   DD DISP=SHR,DSN=&CPYBKS
//         DD DSN=CEE.SCEESAMP,DISP=SHR
//SYSPRINT DD DISP=SHR,DSN=&LISTING(&MEM)
//SYSLIN   DD DSN=&&LOADSET,UNIT=3390,
//            DISP=(MOD,PASS),SPACE=(80,(10,10))
//SYSUT1   DD SPACE=(80,(10,10),,,ROUND),UNIT=3390
//SYSUT2   DD SPACE=(80,(10,10),,,ROUND),UNIT=3390
//SYSUT3   DD SPACE=(80,(10,10),,,ROUND),UNIT=3390
//SYSUT4   DD SPACE=(80,(10,10),,,ROUND),UNIT=3390
//SYSUT5   DD SPACE=(80,(10,10),,,ROUND),UNIT=3390
//SYSUT6   DD SPACE=(80,(10,10),,,ROUND),UNIT=3390
//SYSUT7   DD SPACE=(80,(10,10),,,ROUND),UNIT=3390
//SYSUT8   DD SPACE=(80,(10,10),,,ROUND),UNIT=3390
//SYSUT9   DD SPACE=(80,(10,10),,,ROUND),UNIT=3390
//SYSUT10  DD SPACE=(80,(10,10),,,ROUND),UNIT=3390
//SYSUT11  DD SPACE=(80,(10,10),,,ROUND),UNIT=3390
//SYSUT12  DD SPACE=(80,(10,10),,,ROUND),UNIT=3390
//SYSUT13  DD SPACE=(80,(10,10),,,ROUND),UNIT=3390
//SYSUT14  DD SPACE=(80,(10,10),,,ROUND),UNIT=3390
//SYSUT15  DD SPACE=(80,(10,10),,,ROUND),UNIT=3390
//SYSMDECK DD SPACE=(80,(10,10),,,ROUND),UNIT=3390
//*
//*  *********************************
//*        COPY THE LISTING TO SYSOUT
//*  *********************************
//CBLPRINT  EXEC PGM=IEBGENER,REGION=0M
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=SHR,DSN=&LISTING(&MEM)
//SYSUT2    DD SYSOUT=*
//SYSIN     DD DUMMY
//*
//*  *********************************
//*        LINK-EDIT (BINDER) STEP
//*  *********************************
//*
//LKED EXEC PGM=HEWL,REGION=0M,PARM='LIST,XREF',COND=(8,LT,COMPILE)
//SYSPRINT DD SYSOUT=*
//SYSLIB   DD DSN=SYS1.LINKLIB,DISP=SHR
//         DD DSN=CSF.SCSFMOD0,DISP=SHR
//         DD DSN=CEE.SCEELKED,DISP=SHR
//         DD DSN=CEE.SCEELKEX,DISP=SHR
//         DD DSN=&LOADLIB,DISP=SHR
//SYSLIN   DD DSN=&&LOADSET,DISP=(OLD,DELETE)
//SYSLMOD  DD DISP=SHR,DSN=&LOADLIB(&MEM)
//SYSUT1   DD UNIT=3390,DCB=BLKSIZE=1024,SPACE=(1024,(200,20))
