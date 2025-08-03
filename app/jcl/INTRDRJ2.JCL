//INTRDRJ2 JOB (COBOL),'KSSOMAS',CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),
//         REGION=5M,NOTIFY=&SYSUID
//*********************************************************************
//****  THIS JOB IS TO CREATE PHYSICAL VSAM FILE FOR IMS DEMODB    ****
//****  AND INDEX DEMODX                                           ****
//*********************************************************************
//IDCAMS   EXEC PGM=IDCAMS,DYNAMNBR=200
//SYSPRINT DD SYSOUT=*
//IN DD DSN=AWS.M2.CARDEMO.FTP.TEST.BKUP,DISP=SHR
//OUT DD DSN=AWS.M2.CARDEMO.FTP.TEST.BKUP.INTRDR,DISP=SHR
//SYSIN    DD *
  REPRO IFILE(IN) OFILE(OUT)
/*
//*
