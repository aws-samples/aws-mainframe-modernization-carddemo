//TRANEXTR JOB 'EXTRACT TRAN TYPE',
// CLASS=A,MSGCLASS=0,NOTIFY=&SYSUID
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
//* This JCL will extract reference data for  use in Transaction
//* Report generation. It runs once a day. So changes will reflect
//* in report only after the daily batch is run.
//******************************************************************
//*  Set Parms for this compile:
//*********************************************************************
//   SET HLQ=AWS.M2.CARDDEMO
//*********************************************************************
//****  Db2 Unload using DSNTIAUL utility                        ******
//*********************************************************************
//***   STEP 10 : BACKUP THE PREVIOUS VERSION OF TRANTYPE FILE TO GDG
//*********************************************************************
//STEP10   EXEC PGM=IEBGENER
//SYSPRINT DD   SYSOUT=*
//SYSIN    DD   DUMMY
//SYSUT1   DD   DISP=SHR,DSN=&HLQ..TRANTYPE.PS
//SYSUT2   DD   DSN=&HLQ..TRANTYPE.BKUP(+1),
//       DISP=(NEW,CATLG),
//       DCB=(LRECL=60,RECFM=FB,BLKSIZE=600),
//       SPACE=(TRK,(1,1),RLSE)
//*********************************************************************
//***   STEP 20 : BACKUP THE PREVIOUS VERSION OF TRANCATG FILE TO GDG
//*********************************************************************
//STEP20   EXEC PGM=IEBGENER,COND=(0,NE)
//SYSPRINT DD   SYSOUT=*
//SYSIN    DD   DUMMY
//SYSUT1   DD   DISP=SHR,DSN=&HLQ..TRANCATG.PS
//SYSUT2   DD   DSN=&HLQ..TRANCATG.PS.BKUP(+1),
//       DISP=(NEW,CATLG),
//       DCB=(LRECL=60,RECFM=FB,BLKSIZE=600),
//       SPACE=(TRK,(1,1),RLSE)
//*********************************************************************
//****  STEP 30 : DELETE FILES FROM PREVIOUS RUN                 ******
//*********************************************************************
//STEP30   EXEC PGM=IEFBR14,COND=(0,NE)
//DD01     DD DISP=(MOD,DELETE,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(1,1)),
//            DSN=&HLQ..TRANTYPE.PS
//DD02     DD DISP=(MOD,DELETE,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(1,1)),
//            DSN=&HLQ..TRANCATG.PS
//*********************************************************************
//****  STEP 40 : EXTRACT DATA FROM TRANSACTION TYPE TABLE       ******
//*********************************************************************
//STEP40   EXEC PGM=IKJEFT01,COND=(0,NE)
//STEPLIB  DD  DISP=SHR,DSN=OEM.DB2.DAZ1.RUNLIB.LOAD
//         DD  DISP=SHR,DSN=OEMA.DB2.VERSIONA.SDSNLOAD
//SYSTSPRT DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*
//SYSPUNCH DD  DUMMY
//SYSREC00 DD  DISP=(NEW,CATLG,DELETE),
//             DSN=&HLQ..TRANTYPE.PS,
//             SPACE=(TRK,(1,1),RLSE)
//SYSIN    DD *
 SELECT CAST(CONCAT(CONCAT(
   TR_TYPE
  ,CAST(TR_DESCRIPTION AS CHAR(50))
   )
  ,REPEAT('0',8)
 ) AS CHAR(60))
  FROM
  CARDDEMO.TRANSACTION_TYPE
  ORDER BY TR_TYPE;
/*
//SYSTSIN DD *
  DSN SYSTEM(DAZ1)
  RUN PROGRAM(DSNTIAUL) -
  PLAN(DSNTIAUL) -
  PARMS('SQL')
/*
//*********************************************************************
//****  STEP 50 : EXTRACT DATA FROM TRANSACTION TYPE TABLE       ******
//*********************************************************************
//STEP50   EXEC PGM=IKJEFT01,COND=(4,LT)
//STEPLIB  DD  DISP=SHR,DSN=OEM.DB2.DAZ1.RUNLIB.LOAD
//         DD  DISP=SHR,DSN=OEMA.DB2.VERSIONA.SDSNLOAD
//SYSTSPRT DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*
//SYSPUNCH DD  DUMMY
//SYSREC00 DD  DISP=(NEW,CATLG,DELETE),
//             DSN=&HLQ..TRANCATG.PS,
//             SPACE=(TRK,(1,1),RLSE)
//SYSIN    DD *
  SELECT CAST(
        TRC_TYPE_CODE
     || TRC_TYPE_CATEGORY
     || CAST(TRC_CAT_DATA AS CHAR(50))
     || REPEAT('0',4)
                      AS CHAR(60))
  FROM  CARDDEMO.TRANSACTION_TYPE_CATEGORY
  ORDER BY
        TRC_TYPE_CODE
      , TRC_TYPE_CATEGORY;
/*
//SYSTSIN DD *
  DSN SYSTEM(DAZ1)
  RUN PROGRAM(DSNTIAUL) -
  PLAN(DSNTIAUL) -
  PARMS('SQL')
/*
