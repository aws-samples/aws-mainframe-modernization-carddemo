//OPENFIL JOB 'Open files in CICS',CLASS=A,MSGCLASS=0,
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
//* Open files in CICS region                                                  
//*********************************************************************         
//OPCIFIL EXEC PGM=SDSF                                                         
//ISFOUT DD SYSOUT=*                                                            
//CMDOUT DD SYSOUT=*                                                            
//ISFIN  DD *                                                                   
 /F CICSAWSA,'CEMT SET FIL(TRANSACT ) OPE'                                      
 /F CICSAWSA,'CEMT SET FIL(CCXREF ) OPE'                                        
 /F CICSAWSA,'CEMT SET FIL(ACCTDAT ) OPE'                                       
 /F CICSAWSA,'CEMT SET FIL(CXACAIX ) OPE'                                       
 /F CICSAWSA,'CEMT SET FIL(USRSEC ) OPE'                                       
/*      
//*
//* Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:23:06 CDT
//*
