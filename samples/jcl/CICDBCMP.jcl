//CIDBCMP JOB 'Compile CICS DB2 PGM',CLASS=A,MSGCLASS=H,
//             MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID,TIME=1440
//*********************************************************************
//*  change CICSDB2P to your program name everywhere
//*----->   C CICSDB2P xyz all <--------
//*  set    HLQ  to your high level qualifier
//*  set    SSID to your DB2 SSID  !!!
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
//****  Sample CICS COBOL Db2 compile JCL                        ******
//****  Check with your Administrator for                        ******
//****  JCL suitable to your environment                         ******
//*********************************************************************
//****  Compile CICS COBOL program                               ******
//****  After compiling the related maps                         ******
//*********************************************************************
//*  Set Parms for this compile:
//*********************************************************************
//   SET HLQ=AWS.M2.CARDDEMO
//   SET SSID=DAZ1
//   SET MEMNAME=CICSDB2P
//   SET SRCLIB=&HLQ..CBL
//   SET COPYLIB=&HLQ..CPY
//   SET DCLLIB=&HLQ..DCL
//   SET LOADLIB=&HLQ..LOADLIB
//   SET DBRMLIB=&HLQ..DBRMLIB
//   SET SYSLBLK=3200
//   SET PLAN=CARDDEMO
//*********************************************************************
//*  Add proclib reference
//*********************************************************************
//CCLIBS  JCLLIB ORDER=&HLQ..PROC.UTIL
//*********************************************************************
//*  Make JCL Symbols available to PROC
//*********************************************************************
// EXPORT SYMLIST=*
//*********************************************************************
//*  compile the COBOL code:
//*********************************************************************
//CICSCMP EXEC BLDCIDB2,MEMNAME=&MEMNAME,HLQ=&HLQ,SSID=&SSID,PLAN=&PLAN


