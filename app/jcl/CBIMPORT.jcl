//CBIMPORT JOB 'Import CARDDEMO Data',CLASS=A,MSGCLASS=0,               
// NOTIFY=&SYSUID
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
//* IMPORT CUSTOMER DATA FROM MULTI-RECORD EXPORT FILE AND SPLIT
//* INTO SEPARATE NORMALIZED FILES FOR TARGET SYSTEM
//* *******************************************************************
//STEP01 EXEC PGM=CBIMPORT
//STEPLIB  DD DISP=SHR,
//         DSN=AWS.M2.CARDDEMO.LOADLIB
//*
//* INPUT EXPORT FILE
//*
//EXPFILE  DD DISP=SHR,
//         DSN=AWS.M2.CARDDEMO.EXPORT.DATA
//*
//* OUTPUT NORMALIZED FILES
//*
//CUSTOUT  DD DISP=(NEW,CATLG,DELETE),
//         DSN=AWS.M2.CARDDEMO.CUSTDATA.IMPORT,
//         UNIT=SYSDA,
//         SPACE=(TRK,(50,25),RLSE),
//         DCB=(RECFM=FB,LRECL=500,BLKSIZE=0)
//ACCTOUT  DD DISP=(NEW,CATLG,DELETE),
//         DSN=AWS.M2.CARDDEMO.ACCTDATA.IMPORT,
//         UNIT=SYSDA,
//         SPACE=(TRK,(50,25),RLSE),
//         DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//XREFOUT  DD DISP=(NEW,CATLG,DELETE),
//         DSN=AWS.M2.CARDDEMO.CARDXREF.IMPORT,
//         UNIT=SYSDA,
//         SPACE=(TRK,(25,10),RLSE),
//         DCB=(RECFM=FB,LRECL=50,BLKSIZE=0)
//TRNXOUT  DD DISP=(NEW,CATLG,DELETE),
//         DSN=AWS.M2.CARDDEMO.TRANSACT.IMPORT,
//         UNIT=SYSDA,
//         SPACE=(TRK,(100,50),RLSE),
//         DCB=(RECFM=FB,LRECL=350,BLKSIZE=0)
//*
//* ERROR OUTPUT FILE
//*
//ERROUT   DD DISP=(NEW,CATLG,DELETE),
//         DSN=AWS.M2.CARDDEMO.IMPORT.ERRORS,
//         UNIT=SYSDA,
//         SPACE=(TRK,(10,5),RLSE),
//         DCB=(RECFM=FB,LRECL=132,BLKSIZE=0)
//*
//* SYSTEM OUTPUT
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//*//*
//* Ver: CardDemo_v2.0-41-g02a5b3e-251 Date: 2025-10-02 13:43:17 CDT
//*
