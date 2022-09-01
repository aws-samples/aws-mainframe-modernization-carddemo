//READCUST JOB 'Read Customer Data file',CLASS=A,MSGCLASS=0,
// NOTIFY=&SYUID
//* *******************************************************************
//* RUN THE PROGRAM THAT READS THE CUSTOMER MASTER VSAM FILE
//* *******************************************************************
//STEP05 EXEC PGM=CBCUS01C
//STEPLIB  DD DISP=SHR,
//         DSN=AWS.M2.CARDDEMO.LOADLIB
//CUSTFILE DD DISP=SHR,
//         DSN=AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//*
//* Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:23:07 CDT
//*
