//CARDFILE JOB 'Delete define card data',CLASS=A,MSGCLASS=0,
// NOTIFY=&SYSUID              
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
//*********************************************************************         
//* Close files in CICS region                                                  
//*********************************************************************         
//CLCIFIL EXEC PGM=SDSF                                                         
//ISFOUT DD SYSOUT=*                                                            
//CMDOUT DD SYSOUT=*                                                            
//ISFIN  DD *                                                                   
 /F CICSAWSA,'CEMT SET FIL(CARDDAT ) CLO'                                       
 /F CICSAWSA,'CEMT SET FIL(CARDAIX ) CLO'                                       
/*                                                                              
//*                                                                             
//* *******************************************************************         
//* DELETE CARD DATA VSAM FILE IF ONE ALREADY EXISTS                            
//* *******************************************************************         
//STEP05 EXEC PGM=IDCAMS                                                        
//SYSPRINT DD   SYSOUT=*                                                        
//SYSIN    DD   *                                                               
   DELETE AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS -                                  
          CLUSTER                                                               
   IF MAXCC LE 08 THEN SET MAXCC = 0                                            
   DELETE AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX -                                   
          ALTERNATEINDEX                                                        
   IF MAXCC LE 08 THEN SET MAXCC = 0                                            
/*                                                                              
//*                                                                             
//* *******************************************************************         
//* DEFINE CARD DATA VSAM FILE                                                  
//* *******************************************************************         
//STEP10 EXEC PGM=IDCAMS                                                        
//SYSPRINT DD   SYSOUT=*                                                        
//SYSIN    DD   *                                                               
   DEFINE CLUSTER (NAME(AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS) -                   
          CYLINDERS(1 5) -                                                      
          VOLUMES(AWSHJ1 -                                                      
          ) -                                                                   
          KEYS(16 0) -                                                          
          RECORDSIZE(150 150) -                                                 
          SHAREOPTIONS(2 3) -                                                   
          ERASE -                                                               
          INDEXED -                                                             
          ) -                                                                   
          DATA (NAME(AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS.DATA) -                 
          ) -                                                                   
          INDEX (NAME(AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS.INDEX) -               
          )                                                                     
/*                                                                              
//* *******************************************************************         
//* COPY DATA FROM FLAT FILE TO VSAM FILE                                       
//* *******************************************************************         
//STEP15 EXEC PGM=IDCAMS                                                        
//SYSPRINT DD   SYSOUT=*                                                        
//CARDDATA DD DISP=SHR,                                                         
//         DSN=AWS.M2.CARDDEMO.CARDDATA.PS                                      
//CARDVSAM DD DISP=SHR,                                                         
//         DSN=AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS                               
//SYSIN    DD   *                                                               
   REPRO INFILE(CARDDATA) OUTFILE(CARDVSAM)                                     
/*                                                                              
//*-------------------------------------------------------------------*         
//* CREATE ALTERNATE INDEX ON ACCT ID                                           
//*-------------------------------------------------------------------*         
//STEP40  EXEC PGM=IDCAMS                                                       
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  *                                                                
   DEFINE ALTERNATEINDEX (NAME(AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX)-              
   RELATE(AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS)                    -              
   KEYS(11 16)                                                   -              
   NONUNIQUEKEY                                                  -              
   UPGRADE                                                       -              
   RECORDSIZE(150,150)                                           -              
   VOLUMES(AWSHJ1)                                               -              
   CYLINDERS(5,1))                                               -              
   DATA (NAME(AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX.DATA))           -              
   INDEX (NAME(AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX.INDEX))                        
/*                                                                              
//*-------------------------------------------------------------------*         
//* DEFINE PATH IS USED TO RELATE THE ALTERNATE INDEX TO BASE CLUSTER           
//*-------------------------------------------------------------------*         
//STEP50  EXEC PGM=IDCAMS                                                       
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  *                                                                
  DEFINE PATH                                           -                       
   (NAME(AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX.PATH)        -                       
    PATHENTRY(AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX))                               
/*                                                                              
//*------------------------------------------------------------------           
//* BUILD ALTERNATE INDEX CLUSTER                                               
//*-------------------------------------------------------------------*         
//STEP60  EXEC PGM=IDCAMS                                                       
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  *                                                                
   BLDINDEX                                                      -              
   INDATASET(AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS)                 -              
   OUTDATASET(AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX)                                
/*                                                                              
//*                                                                             
//*********************************************************************         
//* Open files in CICS region                                                   
//*********************************************************************         
//OPCIFIL EXEC PGM=SDSF                                                         
//ISFOUT DD SYSOUT=*                                                            
//CMDOUT DD SYSOUT=*                                                            
//ISFIN  DD *                                                                   
 /F CICSAWSA,'CEMT SET FIL(CARDDAT ) OPE'                                       
 /F CICSAWSA,'CEMT SET FIL(CARDAIX ) OPE'                                       
/*                                                                              
//                                                                              
//*
//* Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:23:04 CDT
//*
