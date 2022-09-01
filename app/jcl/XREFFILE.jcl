//XREFFILE JOB 'Delete define cross ref file',CLASS=A,MSGCLASS=0,
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
//* *******************************************************************         
//* DELETE CARD XREF VSAM FILE IF ONE ALREADY EXISTS                            
//* *******************************************************************         
//STEP05 EXEC PGM=IDCAMS                                                        
//SYSPRINT DD   SYSOUT=*                                                        
//SYSIN    DD   *                                                               
   DELETE AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS -                                  
          CLUSTER                                                               
   IF MAXCC LE 08 THEN SET MAXCC = 0                                            
   DELETE  AWS.M2.CARDDEMO.CARDXREF.VSAM.AIX  -                                 
          ALTERNATEINDEX                                                        
   IF MAXCC LE 08 THEN SET MAXCC = 0                                            
/*                                                                              
//*                                                                             
//* *******************************************************************         
//* DEFINE CARD XREF VSAM FILE                                                  
//* *******************************************************************         
//STEP10 EXEC PGM=IDCAMS                                                        
//SYSPRINT DD   SYSOUT=*                                                        
//SYSIN    DD   *                                                               
   DEFINE CLUSTER (NAME(AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS) -                   
          CYLINDERS(1 5) -                                                      
          VOLUMES(AWSHJ1 -                                                      
          ) -                                                                   
          KEYS(16 0) -                                                          
          RECORDSIZE(50 50) -                                                   
          SHAREOPTIONS(2 3) -                                                   
          ERASE -                                                               
          INDEXED -                                                             
          ) -                                                                   
          DATA (NAME(AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS.DATA) -                 
          ) -                                                                   
          INDEX (NAME(AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS.INDEX) -               
          )                                                                     
/*                                                                              
//* *******************************************************************         
//* COPY DATA FROM FLAT FILE TO VSAM FILE                                       
//* *******************************************************************         
//STEP15 EXEC PGM=IDCAMS                                                        
//SYSPRINT DD   SYSOUT=*                                                        
//XREFDATA DD DISP=SHR,                                                         
//         DSN=AWS.M2.CARDDEMO.CARDXREF.PS                                      
//XREFVSAM DD DISP=SHR,                                                         
//         DSN=AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS                               
//SYSIN    DD   *                                                               
   REPRO INFILE(XREFDATA) OUTFILE(XREFVSAM)                                     
/*                                                                              
//*********************************************************************         
//* CREATE ALTERNATE INDEX ON ACCT ID                                           
//*********************************************************************         
//STEP20  EXEC PGM=IDCAMS                                                       
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  *                                                                
   DEFINE ALTERNATEINDEX (NAME(AWS.M2.CARDDEMO.CARDXREF.VSAM.AIX)-              
   RELATE(AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS)                    -              
   KEYS(11,25)                                                   -              
   NONUNIQUEKEY                                                  -              
   UPGRADE                                                       -              
   RECORDSIZE(50,50)                                             -              
   FREESPACE(10,20)                                              -              
   VOLUMES(AWSHJ1)                                               -              
   CYLINDERS(5,1))                                               -              
   DATA (NAME(AWS.M2.CARDDEMO.CARDXREF.VSAM.AIX.DATA))           -              
   INDEX (NAME(AWS.M2.CARDDEMO.CARDXREF.VSAM.AIX.INDEX))                        
/*                                                                              
//*********************************************************************         
//* DEFINE PATH IS USED TO RELATE THE ALTERNATE INDEX TO BASE CLUSTER           
//*********************************************************************         
//STEP25  EXEC PGM=IDCAMS                                                       
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  *                                                                
  DEFINE PATH                                           -                       
   (NAME(AWS.M2.CARDDEMO.CARDXREF.VSAM.AIX.PATH)        -                       
    PATHENTRY(AWS.M2.CARDDEMO.CARDXREF.VSAM.AIX))                               
/*                                                                              
//*********************************************************************         
//* BUILD ALTERNATE INDEX CLUSTER                                               
//*********************************************************************         
//STEP30  EXEC PGM=IDCAMS                                                       
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  *                                                                
   BLDINDEX                                                      -              
   INDATASET(AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS)                 -              
   OUTDATASET(AWS.M2.CARDDEMO.CARDXREF.VSAM.AIX)                                
/*                                                                              
//*
//* Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:23:09 CDT
//*
