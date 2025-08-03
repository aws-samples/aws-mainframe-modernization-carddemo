//TXT2PDF1 JOB (T,TEST),'TXT2PDF',MSGCLASS=X,CLASS=A,NOTIFY=&SYSUID
//*
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
//* *******************************************************************
//*
//* ********************************************************`***********
//* CONVERT TEXT FILE TO A PDF FILE
//* *******************************************************************
//*
//TXT2PDF  EXEC PGM=IKJEFT1B,DYNAMNBR=50,
//  PARM=(''),
//  COND=(0,NE)
//*
//STEPLIB  DD DISP=SHR,
//         DSN=AWS.M2.LBD.TXT2PDF.LOAD
//*
//SYSEXEC  DD  DISP=SHR,DSN=AWS.M2.LBD.TXT2PDF.EXEC
//*
//INDD     DD  DISP=SHR,DSN=AWS.M2.CARDDEMO.STATEMNT.PS
//*
//SYSPRINT DD  SYSOUT=*
//SYSTSPRT DD  SYSOUT=*
//SYSTSIN  DD  *
 %TXT2PDF BROWSE Y IN DD:INDD +
 OUT 'AWS.M2.CARDDEMO.STATEMNT.PS.PDF'
/*
//*
