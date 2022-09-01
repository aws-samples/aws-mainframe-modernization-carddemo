//TRANCATG JOB 'DEFINE TRAN CATEGORY',                                          
// CLASS=A,MSGCLASS=0,NOTIFY=&SYSUID          
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
//* DELETE TRANSACTION CATEGORY TYPE VSAM FILE IF ONE ALREADY EXISTS            
//* *******************************************************************         
//STEP05 EXEC PGM=IDCAMS                                                        
//SYSPRINT DD   SYSOUT=*                                                        
//SYSIN    DD   *                                                               
   DELETE AWS.M2.CARDDEMO.TRANCATG.VSAM.KSDS -                                  
          CLUSTER                                                               
   SET    MAXCC = 0                                                             
/*                                                                              
//*                                                                             
//* *******************************************************************         
//* DEFINE TRANSACTION CATEGORY TYPE VSAM FILE                                  
//* *******************************************************************         
//STEP10 EXEC PGM=IDCAMS                                                        
//SYSPRINT DD   SYSOUT=*                                                        
//SYSIN    DD   *                                                               
   DEFINE CLUSTER (NAME(AWS.M2.CARDDEMO.TRANCATG.VSAM.KSDS) -                   
          CYLINDERS(1 5) -                                                      
          VOLUMES(AWSHJ1 -                                                      
          ) -                                                                   
          KEYS(6 0) -                                                           
          RECORDSIZE(60 60) -                                                   
          SHAREOPTIONS(2 3) -                                                   
          ERASE -                                                               
          INDEXED -                                                             
          ) -                                                                   
          DATA (NAME(AWS.M2.CARDDEMO.TRANCATG.VSAM.KSDS.DATA) -                 
          ) -                                                                   
          INDEX (NAME(AWS.M2.CARDDEMO.TRANCATG.VSAM.KSDS.INDEX) -               
          )                                                                     
/*                                                                              
//* *******************************************************************         
//* COPY DATA FROM FLAT FILE TO VSAM FILE                                       
//* *******************************************************************         
//STEP15 EXEC PGM=IDCAMS                                                        
//SYSPRINT DD   SYSOUT=*                                                        
//TRANCATG DD DISP=SHR,                                                         
//         DSN=AWS.M2.CARDDEMO.TRANCATG.PS                                      
//TCATVSAM DD DISP=OLD,                                                         
//         DSN=AWS.M2.CARDDEMO.TRANCATG.VSAM.KSDS                               
//SYSIN    DD   *                                                               
   REPRO INFILE(TRANCATG) OUTFILE(TCATVSAM)                                     
/*                                                                              
//*
//* Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:23:08 CDT
//*
