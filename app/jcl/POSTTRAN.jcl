//POSTTRAN JOB 'POSTTRAN',CLASS=A,MSGCLASS=0,                                   
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
//* Process and load daily transaction file and create transaction              
//* category balance and update transaction master vsam                         
//* *******************************************************************         
//STEP15 EXEC PGM=CBTRN02C                                                      
//STEPLIB  DD DISP=SHR,                                                         
//            DSN=AWS.M2.CARDDEMO.LOADLIB                                       
//SYSPRINT DD SYSOUT=*                                                          
//SYSOUT   DD SYSOUT=*                                                          
//TRANFILE DD DISP=SHR,                                                         
//         DSN=AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS                               
//DALYTRAN DD DISP=SHR,                                                         
//         DSN=AWS.M2.CARDDEMO.DALYTRAN.PS                                      
//XREFFILE DD DISP=SHR,                                                         
//         DSN=AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS                               
//DALYREJS DD DISP=(NEW,CATLG,DELETE),                                          
//         UNIT=SYSDA,                                                          
//         DCB=(RECFM=F,LRECL=430,BLKSIZE=0),                                   
//         SPACE=(CYL,(1,1),RLSE),                                              
//         DSN=AWS.M2.CARDDEMO.DALYREJS(+1)                                     
//ACCTFILE DD DISP=SHR,                                                         
//         DSN=AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS                               
//TCATBALF DD DISP=SHR,                                                         
//         DSN=AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS                               
//*
//* Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:23:06 CDT
//*
