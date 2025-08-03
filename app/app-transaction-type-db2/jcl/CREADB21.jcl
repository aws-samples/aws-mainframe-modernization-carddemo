//CREADB2  JOB (DB2CR),'SNJARAO',CLASS=A,MSGCLASS=A,                    00010016
//         TIME=1440,NOTIFY=&SYSUID,TYPRUN=SCAN                         00020021
//******************************************************************    00021025
//* Copyright Amazon.com, Inc. or its affiliates.                       00022025
//* All Rights Reserved.                                                00023025
//*                                                                     00024025
//* Licensed under the Apache License, Version 2.0 (the "License").     00025025
//* You may not use this file except in compliance with the License.    00026025
//* You may obtain a copy of the License at                             00027025
//*                                                                     00028025
//*    http://www.apache.org/licenses/LICENSE-2.0                       00029025
//*                                                                     00029125
//* Unless required by applicable law or agreed to in writing,          00029225
//* software distributed under the License is distributed on an         00029325
//* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,        00029425
//* either express or implied. See the License for the specific         00029525
//* language governing permissions and limitations under the License    00029625
//********************************************************************* 00031002
//****  Create CARDDEMO Database in DAZ1 subsystem (SSID=DAZ1)   ****** 00040021
//********************************************************************* 00040119
//****  You can run the SQLs linked here through SPUFI           ****** 00041019
//****  If you prefer JCL, you can run them from this job        ****** 00041220
//****  You may need to bind the utilities used first though     ****** 00041320
//****  See AWS.M2.CARDDEMO.JCL.UTIL(BINDTIAD) to see how        ****** 00042017
//********************************************************************* 00050002
//*  SET PARMS FOR THIS JOB:                                            00070021
//********************************************************************* 00071021
//   SET CODER=AWS                                                      00072022
//   SET LBNM=&CODER..M2.CARDDEMO                                       00072122
//   SET DB2S=DAZ1                                                      00073021
//********************************************************************* 00075021
//JOBLIB  DD  DSN=OEM.DB2.&DB2S..SDSNLOAD,DISP=SHR                      00080021
//  DD  DSN=OEM.DB2.&DB2S..SDSNLOAD,DISP=SHR                            00090021
//  DD  DSN=CEE.SCEERUN,DISP=SHR                                        00100004
//********************************************************************* 00100123
//****  STEP 00 : Free existing plans and packages               ****** 00100223
//****          : It ends with RC 8 if not existing.             ****** 00100324
//****          : So dont run it if creating new database        ****** 00100424
//********************************************************************* 00100524
//FREEPLN EXEC PGM=IKJEFT01,DYNAMNBR=20                                 00102023
//STEPLIB  DD DISP=SHR,DSN=OEM.DB2.DAZ1.SDSNEXIT                        00103023
//         DD DISP=SHR,DSN=OEMA.DB2.VERSIONA.SDSNLOAD                   00104023
//SYSPRINT DD SYSOUT=*                                                  00105023
//SYSTSPRT DD SYSOUT=*                                                  00106023
//SYSUDUMP DD SYSOUT=*                                                  00107023
//SYSTSIN  DD DISP=SHR,DSN=&LBNM..CNTL(DB2FREE)                         00108023
//********************************************************************* 00110004
//****  STEP 10 : Use Utility DSNTIAD to create the database     ****** 00120019
//****            This uses an existing STOGROUP AWST1STG        ****** 00121018
//****            You would have to create it if not available   ****** 00122023
//********************************************************************* 00130004
//CRCRDDB  EXEC PGM=IKJEFT01,DYNAMNBR=20                                00140004
//STEPLIB  DD DISP=SHR,DSN=OEM.DB2.&DB2S..RUNLIB.LOAD                   00150021
//         DD DISP=SHR,DSN=OEMA.DB2.VERSIONA.SDSNLOAD                   00160019
//SYSTSPRT DD SYSOUT=*                                                  00170004
//SYSUDUMP DD SYSOUT=*                                                  00180004
//SYSPRINT DD SYSOUT=*                                                  00190004
//SYSTSIN  DD DISP=SHR,DSN=&LBNM..CNTL(DB2TIAD1)                        00200021
//SYSIN    DD DISP=SHR,DSN=&LBNM..CNTL(DB2CREAT)                        00250021
//********************************************************************* 00470011
//****  STEP 20 : Load the transaction Type table                ****** 00480019
//****            using DSNTEP4 utility                          ****** 00481019
//********************************************************************* 00490011
//LDTTYPE  EXEC PGM=IEFBR14,COND=(0,NE)                                 00500019
//RUNTEP2  EXEC PGM=IKJEFT01,DYNAMNBR=20                                00510011
//STEPLIB  DD DISP=SHR,DSN=OEM.DB2.&DB2S..RUNLIB.LOAD                   00520021
//         DD DISP=SHR,DSN=OEMA.DB2.VERSIONA.SDSNLOAD                   00530019
//SYSTSPRT DD SYSOUT=*                                                  00540011
//SYSUDUMP DD SYSOUT=*                                                  00550011
//SYSPRINT DD SYSOUT=*                                                  00560011
//SYSTSIN  DD DISP=SHR,DSN=&LBNM..CNTL(DB2TEP41)                        00570021
//SYSIN    DD DISP=SHR,DSN=&LBNM..CNTL(DB2LTTYP)                        00630021
//********************************************************************* 00750015
//****  STEP 30 : Load Transaction Type Category table           ****** 00760019
//****            using DSNTEP4 utility                          ****** 00761019
//********************************************************************* 00770015
//LDTCCAT  EXEC PGM=IKJEFT01,DYNAMNBR=20,COND=(0,NE)                    00790019
//STEPLIB  DD  DISP=SHR,DSN=OEM.DB2.&DB2S..RUNLIB.LOAD                  00800021
//         DD  DISP=SHR,DSN=OEMA.DB2.VERSIONA.SDSNLOAD                  00810015
//SYSTSPRT DD SYSOUT=*                                                  00820015
//SYSUDUMP DD SYSOUT=*                                                  00830015
//SYSPRINT DD SYSOUT=*                                                  00840015
//SYSTSIN  DD DISP=SHR,DSN=&LBNM..CNTL(DB2TEP41)                        00850021
//SYSIN    DD DISP=SHR,DSN=&LBNM..CNTL(DB2LTCAT)                        00910021
