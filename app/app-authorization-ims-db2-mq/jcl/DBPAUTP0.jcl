//DBPAUTP0 JOB 'DBPAUTP0 DB UNLOAD',CLASS=A,MSGCLASS=X,                         
// REGION=0K,TIME=30,NOTIFY=&SYSUID                                             
//*                                                                             
//**********************************************************************        
//*  DELETE OUTPUT DATASET                                                      
//**********************************************************************        
//STEPDEL  EXEC PGM=IEFBR14                                                     
//SYSPRINT DD SYSOUT=*                                                          
//SYSUT1   DD DSN=AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0,                             
//         DISP=(MOD,DELETE),UNIT=SYSDA,SPACE=(TRK,0)                           
//*                                                                             
//**********************************************************************        
//*  DOWNLOAD DBD DBPAUTP0                                                      
//**********************************************************************        
//UNLOAD   EXEC PGM=DFSRRC00,REGION=4M,                                         
//         PARM=(ULU,DFSURGU0,DBPAUTP0)                                         
//* 
//STEPLIB  DD DSN=OEMA.IMS.IMSP.SDFSRESL,DISP=SHR                               
//         DD DSN=AWS.M2.CARDDEMO.LOADLIB,DISP=SHR                              
//DFSRESLB DD DSN=OEMA.IMS.IMSP.SDFSRESL,DISP=SHR                               
//IMS      DD DSN=OEM.IMS.IMSP.PSBLIB,DISP=SHR                                  
//         DD DSN=OEM.IMS.IMSP.DBDLIB,DISP=SHR                                  
//SYSPRINT DD SYSOUT=*                                                          
//* 
//DFSURGU1 DD DSN=AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0,                             
//            DISP=(,CATLG,DELETE),                                             
//            UNIT=SYSDA,                                                       
//            SPACE=(32274,(600,100),RLSE),                                     
//            DCB=(LRECL=27990,RECFM=VB,BLKSIZE=0)                              
//*                                                                             
//DDPAUTP0 DD DISP=SHR,DSN=OEM.IMS.IMSP.PAUTHDB                                 
//DDPAUTX0 DD DISP=SHR,DSN=OEM.IMS.IMSP.PAUTHDBX                                
//DFSVSAMP DD DSN=OEMPP.IMS.V15R01MB.PROCLIB(DFSVSMDB),DISP=SHR                 
//DFSCTL   DD *                                                                 
SBPARM ACTIV=COND                                                               
//SYSUDUMP DD SYSOUT=*,                                                         
//            DCB=(RECFM=FBA,LRECL=133),                                        
//            SPACE=(605,(500,500),RLSE,,ROUND)                                 
//* 
//RECON1 DD DSN=OEM.IMS.IMSP.RECON1,DISP=SHR                                    
//RECON2 DD DSN=OEM.IMS.IMSP.RECON2,DISP=SHR                                    
//RECON3 DD DSN=OEM.IMS.IMSP.RECON3,DISP=SHR                
//*                     
//DFSWRK01  DD DUMMY                                                            
//DFSSRT01  DD DUMMY                                                       //*
//* Ver: CardDemo_v2.0-35-gcfa73b2-245 Date: 2025-04-29 17:01:37 CDT
//*
