//IMSMQCMP JOB ACTINFO1,'Compile CICS IMS MQ',CLASS=A,NOTIFY=&SYSUID
//*
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
//****  Sample IMS MQ CICS COBOL Compile JCL                     ******
//****  Check with your Administrator for                        ******
//****  JCL suitable to your environment                         ******
//*********************************************************************
//   SET HLQ=AWS.M2
//   SET CICSHLQ=CICSTS
//   SET IMSHLQ=IMS
//   SET MQHLQ=MQ
//*
//TRN    EXEC PGM=DFHECP1$,PARM='CICS,DLI,COBOL3'
//STEPLIB  DD DSN=&CICSHLQ..CICS.SDFHLOAD,DISP=SHR
//SYSIN    DD DSN=&HLQ..COBOL.SRC(IMSMQPGM),DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSPUNCH DD DSN=&&SYSCIN,
//            DISP=(,PASS),UNIT=SYSDA,
//            DCB=BLKSIZE=400,
//            SPACE=(400,(400,100))
//*
//COBOL  EXEC PGM=IGYCRCTL,REGION=0M,COND=(4,LT),
//            PARM='NODYNAM,OBJECT,RENT,APOST,MAP,XREF'
//STEPLIB  DD DSNAME=IGY.SIGYCOMP.V63,DISP=SHR
//         DD DSNAME=CEE.SCEERUN,DISP=SHR
//         DD DSNAME=CEE.SCEERUN2,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSLIN   DD DSNAME=&&LOADSET,UNIT=VIO,
//            DISP=(MOD,PASS),SPACE=(TRK,(3,3)),
//            DCB=(BLKSIZE=3200)
//SYSUT1   DD UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT2   DD UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT3   DD UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT4   DD UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT5   DD UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT6   DD UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT7   DD UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT8   DD UNIT=SYSDA,SPACE=(CYL,(1,2))
//SYSUT9   DD UNIT=SYSDA,SPACE=(CYL,(1,2))
//SYSUT10  DD UNIT=SYSDA,SPACE=(CYL,(1,2))
//SYSUT11  DD UNIT=SYSDA,SPACE=(CYL,(1,2))
//SYSUT12  DD UNIT=SYSDA,SPACE=(CYL,(1,2))
//SYSUT13  DD UNIT=SYSDA,SPACE=(CYL,(1,2))
//SYSUT14  DD UNIT=SYSDA,SPACE=(CYL,(1,2))
//SYSUT15  DD UNIT=SYSDA,SPACE=(CYL,(1,2))
//SYSMDECK DD UNIT=SYSDA,SPACE=(CYL,(1,2))
//SYSLIB   DD DSN=CEE.SCEESAMP,DISP=SHR
//         DD DSN=&CICSHLQ..CICS.SDFHCOB,DISP=SHR
//         DD DSN=&MQHLQ..SCSQCOBC,DISP=SHR
//         DD DSN=&HLQ..COBCOPY,DISP=SHR
//SYSIN    DD  DSN=&&SYSCIN,DISP=(OLD,DELETE)
//*
//*  *********************************
//*        COPY LINK STEP
//*  *********************************
//COPYIMS  EXEC PGM=IEBGENER,REGION=0M,COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD *
   ENTRY IMSMQPGM
   NAME IMSMQPGM(R)
//SYSUT2   DD DSN=&&COPYIMS,DISP=(NEW,PASS),
//            DCB=(LRECL=80,BLKSIZE=400,RECFM=FB),
//            UNIT=3390,SPACE=(400,(20,20))
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//*
//COPYLINK EXEC PGM=IEBGENER,COND=(4,LT)
//SYSUT1   DD *
  INCLUDE SYSLIB(DFHELII)
//SYSUT2   DD DSN=&&COPYLINK,DISP=(NEW,PASS),
//            DCB=(LRECL=80,BLKSIZE=400,RECFM=FB),
//            UNIT=3390,SPACE=(400,(20,20))
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//*
//COPYMQ   EXEC PGM=IEBGENER,COND=(4,LT)
//SYSUT1   DD *
 INCLUDE CSQSTUB(CSQCSTUB)
//SYSUT2   DD DSN=&&COPYMQ,DISP=(NEW,PASS),
//            DCB=(LRECL=80,BLKSIZE=400,RECFM=FB),
//            UNIT=3390,SPACE=(400,(20,20))
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//*
//LKED   EXEC PGM=IEWL,REGION=0M,PARM='LIST,XREF',COND=(4,LT)
//SYSLIB   DD DSN=&MQHLQ..SCSQLOAD,DISP=SHR
//         DD DSN=SYS1.LINKLIB,DISP=SHR
//         DD DSN=CEE.SCEELKED,DISP=SHR
//         DD DSN=&CICSHLQ..CICS.SDFHLOAD,DISP=SHR
//         DD DSN=&IMSHLQ..SDFSRESL,DISP=SHR
//CSQSTUB  DD DSN=&MQHLQ..SCSQLOAD,DISP=SHR
//SYSLIN   DD DSN=&&COPYLINK,DISP=(OLD,DELETE)
//         DD DSN=&&COPYMQ,DISP=(OLD,DELETE)
//         DD DSN=&&LOADSET,DISP=(OLD,DELETE)
//         DD DSN=&&COPYIMS,DISP=(OLD,DELETE)
//SYSPRINT DD SYSOUT=*
//SYSLMOD  DD DSNAME=&HLQ..CICSLOAD(IMSMQPGM),
//            DISP=SHR
//SYSUT1   DD UNIT=VIO,SPACE=(TRK,(10,10))