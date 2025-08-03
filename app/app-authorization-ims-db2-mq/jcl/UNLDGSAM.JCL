//UNLDGSAM JOB 'M2APP',CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),
//             REGION=0M,NOTIFY=&SYSUID,TIME=1440
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
//*
//*
//*
//**********************************************************************
//*  EXECUTE IMS PROGRAM
//**********************************************************************
//*
//STEP01  EXEC PGM=DFSRRC00,
//             PARM='DLI,DBUNLDGS,DLIGSAMP,,,,,,,,,,,N'
//STEPLIB    DD DISP=SHR,DSN=OEMA.IMS.IMSP.SDFSRESL
//           DD DISP=SHR,DSN=OEMA.IMS.IMSP.SDFSRESL.V151
//           DD DISP=SHR,DSN=AWS.M2.CARDDEMO.LOADLIB
//DFSRESLB   DD DISP=SHR,DSN=OEMA.IMS.IMSP.SDFSRESL
//*
//IMS        DD DISP=SHR,DSN=OEM.IMS.IMSP.PSBLIB
//           DD DISP=SHR,DSN=OEM.IMS.IMSP.DBDLIB
//*
//PASFILOP   DD DSN=AWS.M2.CARDDEMO.PAUTDB.ROOT.GSAM,
//           DISP=(OLD,KEEP,KEEP)
//*
//PADFILOP   DD DSN=AWS.M2.CARDDEMO.PAUTDB.CHILD.GSAM,
//            DISP=(OLD,KEEP,KEEP)
//*
//DDPAUTP0   DD DSN=OEM.IMS.IMSP.PAUTHDB,DISP=SHR
//DDPAUTX0   DD DSN=OEM.IMS.IMSP.PAUTHDBX,DISP=SHR
//*
//*FSVSAMP   DD DUMMY
//DFSVSAMP DD DISP=SHR,
//            DSN=OEMPP.IMS.V15R01MB.PROCLIB(DFSVSMDB)
//IMSLOGR    DD DUMMY
//IEFRDER    DD DUMMY
//SYSPRINT   DD SYSOUT=*
//SYSUDUMP   DD SYSOUT=*
//IMSERR     DD SYSOUT=*
//*
