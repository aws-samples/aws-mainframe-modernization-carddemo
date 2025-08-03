//READACCT JOB 'READACCT',REGION=8M,CLASS=A,                            JOB05427
//      MSGCLASS=H,NOTIFY=&SYSUID
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
//* *******************************************************************
//* PRE DELETE STEP
//* *******************************************************************
//PREDEL  EXEC PGM=IEFBR14
//DD01     DD DSN=AWS.M2.CARDDEMO.ACCTDATA.PSCOMP,
//            DISP=(MOD,DELETE,DELETE)
//DD02     DD DSN=AWS.M2.CARDDEMO.ACCTDATA.ARRYPS,
//            DISP=(MOD,DELETE,DELETE)
//DD03     DD DSN=AWS.M2.CARDDEMO.ACCTDATA.VBPS,
//            DISP=(MOD,DELETE,DELETE)
//* *******************************************************************
//* RUN THE PROGRAM THAT READS THE ACCOUNT MASTER VSAM FILE
//* *******************************************************************
//STEP05 EXEC PGM=CBACT01C
//STEPLIB  DD DISP=SHR,
//         DSN=AWS.M2.CARDDEMO.LOADLIB
//ACCTFILE DD DISP=SHR,
//         DSN=AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS
//OUTFILE  DD DSN=AWS.M2.CARDDEMO.ACCTDATA.PSCOMP,
//            DISP=(NEW,CATLG,DELETE),
//            DCB=(LRECL=107,RECFM=FB,DSORG=PS,BLKSIZE=0),
//            UNIT=SYSAD,SPACE=(CYL,(1,2),RLSE)
//ARRYFILE DD DSN=AWS.M2.CARDDEMO.ACCTDATA.ARRYPS,
//            DISP=(NEW,CATLG,DELETE),
//            DCB=(LRECL=110,RECFM=FB,DSORG=PS,BLKSIZE=0),
//            UNIT=SYSAD,SPACE=(CYL,(1,2),RLSE)
//VBRCFILE DD DSN=AWS.M2.CARDDEMO.ACCTDATA.VBPS,
//            DISP=(NEW,CATLG,DELETE),
//            DCB=(LRECL=84,RECFM=VB,DSORG=PS,BLKSIZE=0),
//            UNIT=SYSAD,SPACE=(CYL,(1,2),RLSE)
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
