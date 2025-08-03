//CNJBATMP JOB 'Compile Batch COBOL Program',CLASS=A,MSGCLASS=H,        
//             MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID,TIME=1440
//*********************************************************************         
//*  change BATCHPGM to your program name everywhere                            
//*----->   C BATCHPGM xyz all <--------                                        
//*********************************************************************
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
//*********************************************************************    
//****  Sample Batch COBOL Compile JCL                           ******         
//****  Check with your Administrator for                        ******         
//****  JCL suitable to your environment                         ******
//*********************************************************************    
//*  change BATCHPGM to your program name everywhere                            
//*----->   C BATCHPGM xyz all <--------                                        
//*********************************************************************         
//****  COMPILE BATCH COBOL PROGRAM                              ******         
//*********************************************************************         
//*  Set Parms for this compile:                                                
//*********************************************************************         
//   SET MEMNAME=BATCHPGM                                                       
//   SET HLQ=AWS.M2                                                             
//*********************************************************************         
//*  Add proclib reference                                                      
//*********************************************************************         
//CCLIBS  JCLLIB ORDER=&HLQ..CARDDEMO.PRC.UTIL                                  
//*********************************************************************         
//*  compile the COBOL code:                                                    
//*********************************************************************         
//BATCMP       EXEC BUILDBAT,MEM=&MEMNAME,HLQ=&HLQ                              
