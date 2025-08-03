//CREASTMT JOB 'Create Statement',CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),
//         NOTIFY=&SYSUID,TIME=1440
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
//* This JCL will create statement for each CARD present in the XREF
//* file
//******************************************************************
//DELDEF01 EXEC PGM=IDCAMS
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
  DELETE    AWS.M2.CARDDEMO.TRXFL.SEQ
  DELETE    AWS.M2.CARDDEMO.TRXFL.VSAM.KSDS                     -
            CLUSTER
  SET       MAXCC = 0
  DEFINE    CLUSTER  (NAME(AWS.M2.CARDDEMO.TRXFL.VSAM.KSDS)     -
                      KEYS(32 0)                                -
                      VOLUMES(TSU023)                           -
                      RECORDSIZE(350 350)                       -
                      SHAREOPTIONS(2 3)                         -
                      ERASE                                     -
                      INDEXED                                   -
                      CYL(1 5))                                 -
            DATA      (NAME(AWS.M2.CARDDEMO.TRXFL.DATA)         -
                      CISZ(4096))                               -
            INDEX     (NAME(AWS.M2.CARDDEMO.TRXFL.INDEX))
/*
//*********************************************************************
//* CREATE COPY OF TRANSACT FILE WITH CARD NUMBER AND TRAN ID AS KEY
//*********************************************************************
//STEP010  EXEC PGM=SORT
//SORTIN   DD  DISP=SHR,DSN=AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS
//SYSPRINT DD  SYSOUT=*
//SYSOUT   DD  SYSOUT=*
//SORTOUT  DD  DSN=AWS.M2.CARDDEMO.TRXFL.SEQ,
//             DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//             DCB=(LRECL=350,BLKSIZE=3500,RECFM=FB),
//             SPACE=(CYL,(1,1),RLSE)
//SYSIN    DD *
  SORT FIELDS=(263,16,CH,A,1,16,CH,A)
  OUTREC FIELDS=(1:263,16,17:1,262,279:279,50)
/*
//STEP020  EXEC PGM=IDCAMS,COND=(0,NE)
//SYSPRINT DD  SYSOUT=*
//INFILE   DD  DISP=SHR,DSN=AWS.M2.CARDDEMO.TRXFL.SEQ
//OUTFILE  DD  DISP=SHR,DSN=AWS.M2.CARDDEMO.TRXFL.VSAM.KSDS
//SYSIN    DD  *
  REPRO INFILE(INFILE) OUTFILE(OUTFILE)
/*
//*********************************************************************
//* DELETE TRANSACTION REPORTS FROM PREVIOUS RUN
//*********************************************************************
//STEP030  EXEC PGM=IEFBR14,COND=(0,NE)
//HTMLFILE DD DISP=(MOD,DELETE,DELETE),
//         UNIT=SYSDA,
//         DCB=(LRECL=80,BLKSIZE=3200,RECFM=FB),
//         SPACE=(CYL,(1,1),RLSE),
//         DSN=AWS.M2.CARDDEMO.STATEMNT.HTML
//STMTFILE DD DISP=(MOD,DELETE,DELETE),
//         DCB=(LRECL=80,BLKSIZE=8000,RECFM=FB),
//         SPACE=(CYL,(1,1),RLSE),
//         DSN=AWS.M2.CARDDEMO.STATEMNT.PS
//*********************************************************************
//* PRODUCING REPORT IN TEXT AND HTML - DEMONSTRATES CALLED SUBROUTINE
//*********************************************************************
//STEP040  EXEC PGM=CBSTM03A,COND=(0,NE)
//STEPLIB  DD  DISP=SHR,DSN=AWS.M2.CARDDEMO.LOADLIB
//SYSPRINT DD  SYSOUT=*
//SYSOUT   DD  SYSOUT=*
//TRNXFILE DD  DISP=SHR,DSN=AWS.M2.CARDDEMO.TRXFL.VSAM.KSDS
//XREFFILE DD  DISP=SHR,DSN=AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS
//ACCTFILE DD  DISP=SHR,DSN=AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS
//CUSTFILE DD  DISP=SHR,DSN=AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS
//STMTFILE DD DISP=(NEW,CATLG,DELETE),
//         UNIT=SYSDA,
//         DCB=(LRECL=80,BLKSIZE=8000,RECFM=FB),
//         SPACE=(CYL,(1,1),RLSE), 00,RECFM=FB), ATA.VSAM.KSDS
//         DSN=AWS.M2.CARDDEMO.STATEMNT.PS
//HTMLFILE DD  DISP=(NEW,CATLG,DELETE),
//         UNIT=SYSDA,
//         DCB=(LRECL=100,BLKSIZE=800,RECFM=FB),
//         SPACE=(CYL,(1,1),RLSE),
//         DSN=AWS.M2.CARDDEMO.STATEMNT.HTML
//*
