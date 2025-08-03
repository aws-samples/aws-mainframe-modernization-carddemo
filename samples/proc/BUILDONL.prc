//*********************************************************************
//*  THIS PROC IS USED TO COMPILE CICS PGMS
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
//****  Sample CICS COBOL Compile PROC                           ******         
//****  Run after related BMS is compiled                        ******         
//****  Check with your Administrator for                        ******         
//****  JCL suitable to your environment                         ******
//*********************************************************************
//*
//BLDONL PROC MEM=,
//            HLQ=,
//            COBLIB=IGY.SIGYCOMP.V63,
//            DFHLOAD=OEM.CICSTS.V05R06M0.CICS.SDFHLOAD,
//            DFHCOB=OEM.CICSTS.V05R06M0.CICS.SDFHCOB,
//            DFHSAMP=OEM.CICSTS.V05R06M0.CICS.SDFHSAMP,
//            CPYBKS=&HLQ..CARDDEMO.CPY,
//            SOURCE=&HLQ..CARDDEMO.CBL,
//            LOADLIB=&HLQ..CARDDEMO.LOADLIB,
//            LISTING=&HLQ..CARDDEMO.LST
//*
//*  ---------------------------
//*  COBOL PROGRAM COMPILE STEP
//*  ---------------------------
//COB     EXEC PGM=IGYCRCTL,REGION=0M,
//   PARM=(CICS,NODYNAM,RENT,NOSEQ)
//STEPLIB  DD DSN=&COBLIB,DISP=SHR
//         DD DSN=&DFHLOAD,DISP=SHR
//SYSLIB   DD DSN=&CPYBKS,DISP=SHR
//         DD DSN=&DFHCOB,DISP=SHR
//         DD DSN=CEE.SCEESAMP,DISP=SHR                                         
//*YSPRINT DD SYSOUT=*
//SYSPRINT DD DSN=&LISTING(&MEM),DISP=SHR
//SYSIN    DD DSN=&SOURCE(&MEM),DISP=SHR
//SYSLIN   DD DSN=&&LOADSET,DISP=(MOD,PASS),
//            UNIT=3390,SPACE=(80,(250,100))
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
//*  *********************************
//*      REPLICATE LISTING ON SYSOUT
//*  *********************************
//DISPLIST EXEC PGM=IEBGENER,REGION=0M
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=&LISTING(&MEM),DISP=SHR
//SYSUT2   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//*  *********************************
//*        COPY LINK STEP
//*  *********************************
//COPYLINK EXEC PGM=IEBGENER,REGION=0M
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=&DFHSAMP(DFHEILID),DISP=SHR
//SYSUT2   DD DSN=&&COPYLINK,DISP=(NEW,PASS),
//            DCB=(LRECL=80,BLKSIZE=400,RECFM=FB),
//            UNIT=3390,SPACE=(400,(20,20))
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//*
//*  *********************************
//*        LINK-EDIT (BINDER) STEP
//*  *********************************
//LKED EXEC PGM=HEWL,REGION=0M,PARM='LIST,XREF,LET,MAP,AMODE(31),
//                                   RMODE(ANY)'
//*KED EXEC PGM=HEWL,REGION=0M,PARM='LIST,XREF,LET,DCBS,AMODE(31),
//*                                  RMODE(ANY)'
//SYSPRINT DD SYSOUT=*
//SYSLIB   DD DSN=&DFHLOAD,DISP=SHR
//         DD DSN=SYS1.LINKLIB,DISP=SHR
//         DD DSN=CSF.SCSFMOD0,DISP=SHR
//         DD DSN=CEE.SCEELKED,DISP=SHR
//         DD DSN=CEE.SCEELKEX,DISP=SHR
//         DD DSN=&LOADLIB,DISP=SHR
//SYSLIN   DD DSN=&&LOADSET,DISP=(OLD,DELETE)
//         DD DSN=&&COPYLINK,DISP=(OLD,DELETE)
//SYSLMOD  DD DSN=&LOADLIB(&MEM),
//*           DISP=(OLD,KEEP),SPACE=(CYL,(10,20,10)),
//*           UNIT=3390,DSNTYPE=LIBRARY
//            DISP=SHR
//SYSUT1   DD UNIT=3390,DCB=BLKSIZE=1024,SPACE=(1024,(200,20))
//*
