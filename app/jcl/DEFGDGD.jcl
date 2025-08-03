//DEFGDGD JOB 'DEF DB2 GDG',CLASS=A,MSGCLASS=0,NOTIFY=&SYSUID           JOB05067
//*  RESTART=STEP30                                                     JOB05067
//******************************************************************
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
//******************************************************************
//* This jcl will create GDGs and load first generation for
//* Transaction reference data
//* *******************************************************************
//*  Define GDG for Transaction Type
//* *******************************************************************
//STEP10 EXEC PGM=IDCAMS
//SYSPRINT DD   SYSOUT=*
//SYSIN    DD   *
   DEFINE GENERATIONDATAGROUP -
   (NAME(AWS.M2.CARDDEMO.TRANTYPE.BKUP) -
    LIMIT(5) -
    SCRATCH -
   )
/*
//* *******************************************************************
//*  Create the first generation of GDG Transaction Type
//* *******************************************************************
//STEP20   EXEC PGM=IEBGENER,COND=(0,NE)
//SYSPRINT DD   SYSOUT=*
//SYSIN    DD   DUMMY
//SYSUT1   DD   DISP=SHR,DSN=AWS.M2.CARDDEMO.TRANTYPE.PS
//SYSUT2   DD   DSN=AWS.M2.CARDDEMO.TRANTYPE.BKUP(+1),
//       DISP=(NEW,CATLG),
//       DCB=(LRECL=60,RECFM=FB,BLKSIZE=600),
//       SPACE=(TRK,(1,1),RLSE)
//* *******************************************************************
//*  Transaction Category type
//* *******************************************************************
//STEP30 EXEC PGM=IDCAMS,COND=(0,NE)
//SYSPRINT DD   SYSOUT=*
//SYSIN    DD   *
   DEFINE GENERATIONDATAGROUP -
   (NAME(AWS.M2.CARDDEMO.TRANCATG.PS.BKUP) -
    LIMIT(5) -
    SCRATCH -
   )
/*
//* *******************************************************************
//*  Create the first generation of GDG Transaction Category Type
//* *******************************************************************
//STEP40   EXEC PGM=IEBGENER,COND=(0,NE)
//SYSPRINT DD   SYSOUT=*
//SYSIN    DD   DUMMY
//SYSUT1   DD   DISP=SHR,DSN=AWS.M2.CARDDEMO.TRANCATG.PS
//SYSUT2   DD   DSN=AWS.M2.CARDDEMO.TRANCATG.PS.BKUP(+1),
//       DISP=(NEW,CATLG),
//       DCB=(LRECL=60,RECFM=FB,BLKSIZE=600),
//       SPACE=(TRK,(1,1),RLSE)
//* *******************************************************************
//*  Disclosure Group
//* *******************************************************************
//STEP50 EXEC PGM=IDCAMS
//SYSPRINT DD   SYSOUT=*
//SYSIN    DD   *
   DEFINE GENERATIONDATAGROUP -
   (NAME(AWS.M2.CARDDEMO.DISCGRP.BKUP) -
    LIMIT(5) -
    SCRATCH -
   )
/*
//* *******************************************************************
//*  Create the first generation of GDG disclosure group
//* *******************************************************************
//STEP60   EXEC PGM=IEBGENER,COND=(0,NE)
//SYSPRINT DD   SYSOUT=*
//SYSIN    DD   DUMMY
//SYSUT1   DD   DISP=SHR,DSN=AWS.M2.CARDDEMO.DISCGRP.PS
//SYSUT2   DD   DSN=AWS.M2.CARDDEMO.DISCGRP.BKUP(+1),
//       DISP=(NEW,CATLG),
//       DCB=(LRECL=50,RECFM=FB,BLKSIZE=500),
//       SPACE=(TRK,(1,1),RLSE)
//
//
//*
//* Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:23:04 CDT
//*
