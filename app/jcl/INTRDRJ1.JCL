//INTRDRJ1 JOB (COBOL),'KSSOMAS',CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),
//         REGION=5M,NOTIFY=&SYSUID
//*********************************************************************
//*** THIS INTRDR JOB WILL TRIGGER ANOTHER JCL
//*********************************************************************
//IDCAMS   EXEC PGM=IDCAMS,DYNAMNBR=200
//SYSPRINT DD SYSOUT=*
//IN DD DSN=AWS.M2.CARDEMO.FTP.TEST,DISP=SHR
//OUT DD DSN=AWS.M2.CARDEMO.FTP.TEST.BKUP,DISP=SHR
//SYSIN    DD *
  REPRO IFILE(IN) OFILE(OUT)
/*
//*********************************************************************
//STEP01  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN DD DUMMY
//SYSUT1 DD DSN=AWS.M2.CARDDEMO.JCL(INTRDRJ2),DISP=SHR
//SYSUT2 DD SYSOUT=(A,INTRDR),DCB=(LRECL=80,BLKSIZE=80)
//*
