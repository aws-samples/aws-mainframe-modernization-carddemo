//AUDPOP01 JOB 'AUDIT LOG POPULATION',CLASS=A,MSGCLASS=0,                 
// NOTIFY=&SYSUID      
//*
//* AUDPOP01 - Audit Log Population Batch Job
//* 
//* Purpose: Execute CBAUDP01 COBOL program to generate audit log
//*          entries from customer, account, transaction, and card records
//*
//* Input:  Data for customer, account, transaction, and card files
//* Output: Audit log file with generated audit records
//*
//STEP01   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE AWS.M2.CARDDEMO.AUDIT.NEW
  SET MAXCC = 0
//*
//STEP02   EXEC PGM=CBAUDP01
//STEPLIB  DD DSN=CARDDEMO.LOADLIB,DISP=SHR
//SYSOUT   DD   SYSOUT=*
//SYSPRINT DD   SYSOUT=*
//SYSUDUMP DD   SYSOUT=*
//CUSTIN   DD DSN=AWS.M2.CARDDEMO.CUSTDATA.PS,DISP=SHR
//ACCTIN   DD DSN=AWS.M2.CARDDEMO.ACCTDATA.PS,DISP=SHR
//TRANIN   DD DSN=AWS.M2.CARDDEMO.DALYTRAN.PS,DISP=SHR
//CARDIN   DD DSN=AWS.M2.CARDDEMO.CARDDATA.PS,DISP=SHR
//AUDOUT   DD DSN=AWS.M2.CARDDEMO.AUDIT.NEW,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=565,BLKSIZE=0)