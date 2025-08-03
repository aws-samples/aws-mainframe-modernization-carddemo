//CBPAUP0J JOB 'CARDDEMO',CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),
//             REGION=0M,NOTIFY=&SYSUID
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
//**********************************************************************
//*  EXECUTE IMS PROGRAM TO DELETE EXPIRED AUTHORIZATIONS
//**********************************************************************
//*
//STEP01  EXEC PGM=DFSRRC00,
//             PARM='BMP,CBPAUP0C,PSBPAUTB'
//STEPLIB    DD DISP=SHR,DSN=IMS.SDFSRESL
//           DD DISP=SHR,DSN=XXXXXXXX.PROD.LOADLIB
//DFSRESLB   DD DISP=SHR,DSN=IMS.SDFSRESL
//PROCLIB    DD DISP=SHR,DSN=IMS.PROCLIB
//*
//DFSSEL     DD DSN=IMS.SDFSRESL,DISP=SHR
//*
//IMS        DD DISP=SHR,DSN=IMS.PSBLIB
//           DD DISP=SHR,DSN=IMS.DBDLIB
//*
//SYSIN    DD *
00,00001,00001,Y
//*
//SYSOUX     DD SYSOUT=*
//SYSOUT     DD SYSOUT=*
//SYSABOUT   DD SYSOUT=*
//ABENDAID   DD SYSOUT=*
//IEFRDER    DD DUMMY
//IMSLOGR    DD DUMMY
//SYSPRINT   DD SYSOUT=*
//SYSUDUMP   DD SYSOUT=*
//IMSERR     DD SYSOUT=*