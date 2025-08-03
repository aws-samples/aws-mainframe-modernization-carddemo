//*********************************************************************
//*  THIS PROC IS USED TO COMPILE CICS + DB2 PROGRAMS
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
//****  Sample CICS COBOL Db2 compile proc                       ******
//****  Run after related BMS is compiled                        ******
//****  Check with your Administrator for                        ******
//****  JCL suitable to your environment                         ******
//*********************************************************************
//*
//BLDONL PROC MEMNAME=,
//            HLQ=,
//            SSID=,
//            PLAN=,
//            SDSNEXIT=OEM.DB2.&SSID..SDSNEXIT,
//            SDSNLOAD=OEM.DB2.&SSID..SDSNLOAD,
//            SDFHLOAD=OEM.CICSTS.V05R06M0.CICS.SDFHLOAD,
//            SDFHCOB=OEM.CICSTS.V05R06M0.CICS.SDFHCOB,
//            SDFHMAC=OEM.CICSTS.V05R06M0.CICS.SDFHMAC,
//            DBRMLIB=&HLQ..DBRMLIB,
//            LIBPRFX=CEE,
//            COBLIB=IGY.SIGYCOMP.V63,
//            CPYBKS=&HLQ..CPY,
//            SRCLIB=&HLQ..CBL,
//            LOADLIB=&HLQ..LOADLIB
//*           LISTING=&HLQ..LST
//*
//*********************************************************
//*       Db2 Precompile the Cobol Program
//*       Draw all SQL statements out into DBRMLIB
//*********************************************************
//PRECMP  EXEC PGM=DSNHPC,
//             PARM='HOST(IBMCOB),XREF,SOURCE,FLAG(I),APOST'
//STEPLIB  DD  DSN=&SDSNEXIT,DISP=SHR
//         DD  DSN=&SDSNLOAD,DISP=SHR
//SYSIN    DD  DISP=SHR,DSN=&SRCLIB(&MEMNAME)
//SYSCIN   DD  DSN=&&DSNHOUT,UNIT=VIO,
//             DISP=(NEW,PASS),SPACE=(TRK,(3,3)),
//             DCB=(BLKSIZE=&SYSLBLK)
//DBRMLIB  DD  DISP=OLD,DSN=&DBRMLIB(&MEMNAME)
//SYSLIB   DD  DISP=SHR,DSN=&DCLLIB
//         DD  DISP=SHR,DSN=&COPYLIB
//         DD  DISP=SHR,DSN=&CPYBKS
//SYSPRINT DD  SYSOUT=*
//SYSTERM  DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*
//SYSUT1   DD  SPACE=(80,(10,10),,,ROUND),UNIT=VIO
//SYSUT2   DD  SPACE=(80,(10,10),,,ROUND),UNIT=VIO
//*
//*********************************************************
//*       Perform CICS translation
//*       Replace CICS command blocks with call statements
//*********************************************************
//TRN    EXEC  PGM=DFHECP1$,PARM='NOSEQ',
//             REGION=2048K,COND=(0,NE,PRECMP)
//STEPLIB  DD  DISP=SHR,DSN=&SDFHLOAD
//SYSIN    DD DSN=&&DSNHOUT,DISP=(OLD,DELETE)
//SYSPRINT DD  SYSOUT=*
//SYSPUNCH DD  DSN=&&SYSCIN,
//             DISP=(,PASS),UNIT=SYSDA,
//             DCB=BLKSIZE=400,
//             SPACE=(400,(400,100))
//*********************************************************
//*       Perform COBOL Compilation
//*********************************************************
//COBOL  EXEC PGM=IGYCRCTL,REGION=2048K,
//         PARM='RENT,NODYNAM,NOSEQ',COND=(0,NE)
//STEPLIB  DD DISP=SHR,DSN=&COBLIB
//SYSLIB   DD DISP=SHR,DSN=&DCLLIB
//         DD DISP=SHR,DSN=&COPYLIB
//         DD DISP=SHR,DSN=&CPYBKS
//         DD DISP=SHR,DSN=&SDFHCOB
//         DD DISP=SHR,DSN=&SDFHMAC
//SYSPRINT DD SYSOUT=*
//SYSIN DD DSN=&&SYSCIN,DISP=(OLD,DELETE)
//SYSLIN   DD DSN=&&LOADSET,UNIT=VIO,
//            DISP=(MOD,PASS),SPACE=(TRK,(3,3)),
//            DCB=(BLKSIZE=&SYSLBLK)
//SYSUT1   DD UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT2   DD UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT3   DD UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT4   DD UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT5   DD UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT6   DD UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT7   DD UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT8   DD UNIT=SYSDA,SPACE=(CYL,(1,2))
//SYSUT9   DD UNIT=SYSDA,SPACE=(CYL,(1,2))
//SYSUT10  DD UNIT=SYSDA,SPACE=(CYL,(1,2))
//SYSUT11  DD UNIT=SYSDA,SPACE=(CYL,(1,2))
//SYSUT12  DD UNIT=SYSDA,SPACE=(CYL,(1,2))
//SYSUT13  DD UNIT=SYSDA,SPACE=(CYL,(1,2))
//SYSUT14  DD UNIT=SYSDA,SPACE=(CYL,(1,2))
//SYSUT15  DD UNIT=SYSDA,SPACE=(CYL,(1,2))
//SYSMDECK DD UNIT=SYSDA,SPACE=(CYL,(1,2))
//COND01 IF RC = 0 THEN
//*********************************************************
//*       Link Edit
//*       Combine all the object modules to a single module
//*********************************************************
//LKED   EXEC PGM=HEWL,COND=(0,NE),REGION=1024K
//SYSLIB   DD DISP=SHR,DSN=&LIBPRFX..SCEELKEX
//         DD DISP=SHR,DSN=&LIBPRFX..SCEELKED
//         DD DISP=SHR,DSN=&SDFHLOAD
//         DD DISP=SHR,DSN=&SDFHCOB
//         DD DISP=SHR,DSN=&SDSNLOAD
//         DD DISP=SHR,DSN=ISP.SISPLOAD
//         DD DISP=SHR,DSN=GDDM.SADMMOD
//         DD DSN=&LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSLIN   DD DISP=SHR,DSN=&SDFHCOB(DFHEILIC)
//         DD DISP=(OLD,DELETE),DSN=&&LOADSET
//         DD DDNAME=SYSIN
//SYSLMOD  DD DSNAME=&LOADLIB(&MEMNAME),
//            SPACE=(TRK,(10,10,1)),
//            UNIT=VIO,DISP=SHR
//SYSUT1   DD UNIT=VIO,SPACE=(TRK,(10,10))
//SYSIN    DD DUMMY
//*********************************************************************
//*       Bind all DBRMs into a single collection
//*********************************************************************
//BINDPK  EXEC PGM=IKJEFT01,COND=(0,NE)
//STEPLIB  DD DISP=SHR,DSN=OEM.DB2.DAZ1.SDSNEXIT
//         DD DISP=SHR,DSN=OEMA.DB2.VERSIONA.SDSNLOAD
//DBRMLIB  DD DISP=SHR,DSN=&DBRMLIB(&MEMNAME)
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//SYSTSIN  DD *,SYMBOLS=JCLONLY
 DSN     SYSTEM    (&SSID)
 BIND    PACKAGE   (CARDDEMO) -
         MEMBER    (&MEMNAME) -
         QUALIFIER (CARDDEMO) -
         ACTION    (REPLACE)  -
         ISOLATION (CS)       -
         CURRENTDATA(YES)     -
         EXPLAIN   (NO)       -
         ENCODING  (EBCDIC)
 END
/*
//*********************************************************************
//*       Bind Package to CardDemo Plan
//*       This plan must match with what we have defined in DB2ENTRY
//*********************************************************************
//BINDPL  EXEC PGM=IKJEFT01,COND=(4,LT,BINDPK)
//STEPLIB  DD DISP=SHR,DSN=OEM.DB2.DAZ1.SDSNEXIT
//         DD DISP=SHR,DSN=OEMA.DB2.VERSIONA.SDSNLOAD
//DBRMLIB  DD DISP=SHR,DSN=&DBRMLIB(&MEMNAME)
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//DEBUGLST DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//SYSTSIN  DD *,SYMBOLS=JCLONLY
 DSN     SYSTEM    (&SSID)
 BIND    PLAN      (&PLAN) -
         PKLIST    (CARDDEMO.*) -
         QUALIFIER (CARDDEMO) -
         ACTION    (REPLACE)  RETAIN   -
         ISOLATION (CS)       -
         CURRENTDATA(YES)     -
         ENCODING  (EBCDIC)
 END
/*
//       ENDIF
