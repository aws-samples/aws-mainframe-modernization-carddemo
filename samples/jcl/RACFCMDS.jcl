//RACFCMDS JOB ACTINFO1,'RACF',CLASS=A,MSGCLASS=Y,
// MSGLEVEL=(1,1),NOTIFY=&SYSUID,TIME=1440        
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
//TSOBAT EXEC PGM=IKJEFT01,REGION=6M                                            
//*                                                                             
//SYSTSPRT DD SYSOUT=*                                                          
//* Adding a transaction to a profile (CARD)                                    
//SYSTSIN  DD *                                                                 
 RALT GCICSTRN CARD ADDMEM(CT02)                                                
/*                                                                              
//                                                                              
//* Add user to a group.                                                        
//SYSTSIN  DD *                                                                 
 CONNECT AWSCODR GROUP(M2APPDEV)                                                
/*                                                                              
//                                                                              
//*
//* Ver: CardDemo_v1.0-57-g40b7caa-110 Date: 2022-08-19 18:00:43 CDT
//*
