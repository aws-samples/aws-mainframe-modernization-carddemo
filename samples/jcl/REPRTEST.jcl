//REPRTEST JOB 'REPRO TEST JOB',CLASS=A,MSGCLASS=0,                             
//  NOTIFY=&SYSUID                     
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
//* CALL THE REPRO PROC                                                         
//* *******************************************************************         
//JOBLIB JCLLIB ORDER=('AWS.M2.CARDDEMO.PROC')                                  
//STEP05 EXEC PROC=REPROC,                                                      
// CNTLLIB=AWS.M2.CARDDEMO.CNTL                                                 
//*                                                                             
//PRC001.FILEIN  DD DISP=SHR,                                                   
//        DSN=AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS                                
//*                                                                             
//PRC001.FILEOUT DD DISP=(NEW,CATLG,DELETE),                                    
//        UNIT=SYSDA,                                                           
//        DCB=(LRECL=350,RECFM=FB,BLKSIZE=0),                                   
//        SPACE=(CYL,(1,1),RLSE),                                               
//        DSN=AWS.M2.CARDDEMO.TRANSACT.BKUP(+1)                                 
//*
//* Ver: CardDemo_v1.0-57-g40b7caa-110 Date: 2022-08-19 18:00:43 CDT
//*
