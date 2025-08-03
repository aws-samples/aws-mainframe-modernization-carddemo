//MNTTRDB2 JOB (COBOL),'MNTTRDB2',CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),    JOB09214
//         NOTIFY=&SYSUID,TIME=1440
//*******************************************************************//
//*                                                                 *//
//* USE THIS JOB TO MAINTAIN DB2 TRANSACTION TABLE, INSERTING,      *//
//* UPDATING OR REMOVING RECORDS IN BATCH.                          *//
//*                                                                 *//
//* INPFILE - THE INPUT FILE TO USE FOR UPDATES. THE ALLOWED VALUES *//
//*           ARE:                                                  *//
//*                                                                 *//
//* COLUMN 1     - A - ADD                                          *//
//*                D - DELETE                                       *//
//*                U - UPDATE                                       *//
//*                * - COMMENT                                      *//
//*                                                                 *//
//* COLUMNS 2-3  - TRANSACTION TYPE. NUMERIC VALUE                  *//
//*                                                                 *//
//* COLUMNS 4-53 - TRANSACTION DESCRIPTION                          *//
//*                                                                 *//
//*******************************************************************//
//STEP1   EXEC PGM=IKJEFT01,REGION=0M
//STEPLIB  DD DISP=SHR,DSN=OEM.DB2.DAZ1.SDSNEXIT
//         DD DISP=SHR,DSN=OEMA.DB2.VERSIONA.SDSNLOAD
//         DD DISP=SHR,DSN=AWS.M2.CARDDEMO.LOADLIB
//DBRMLIB  DD DISP=SHR,DSN=AWS.M2.CARDDEMO.DBRMLIB
//SYSTSPRT DD  SYSOUT=*
//INPFILE  DD  DSN=INPFILE,DISP=SHR
//SYSTSIN DD *
     DSN SYSTEM(DAZ1)
          RUN PROGRAM(COBTUPDT) PLAN(CARDDEMO)