//DEFAUDIT JOB 'DEL/DEF AUDIT VSAM',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)
//*
//* Delete and Define VSAM cluster for audit trail logging
//* Key: AUDIT-LOG-TYPE (1) + AUDIT-TIMESTAMP (26) 
//*                         + AUDIT-USER-ID (8) = 35 bytes
//* Record Length: 565 bytes (fixed)
//*
//STEP1    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE AWS.M2.CARDDEMO.AUDIT.VSAM.KSDS CLUSTER PURGE
  SET MAXCC = 0
  
  DEFINE CLUSTER (                           -
    NAME(AWS.M2.CARDDEMO.AUDIT.VSAM.KSDS)    -
    RECORDSIZE(565 565)                      -
    KEYS(35 0)                               -
    CYLINDERS(5 2)                           -
    FREESPACE(10 5)                          -
    INDEXED                                  -
    SHAREOPTIONS(2 3)                        -
    SPEED                                    -
  ) -
  DATA (                                     -
    NAME(AWS.M2.CARDDEMO.AUDIT.VSAM.KSDS.DATA) -
    CONTROLINTERVALSIZE(4096)                -
  ) -
  INDEX (                                    -
    NAME(AWS.M2.CARDDEMO.AUDIT.VSAM.KSDS.INDEX) -
    CONTROLINTERVALSIZE(512)                 -
  )
  
  IF LASTCC = 0 THEN -
    LISTCAT ENTRIES(AWS.M2.CARDDEMO.AUDIT.VSAM.KSDS) ALL
/*