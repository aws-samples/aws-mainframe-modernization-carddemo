//CICCMP  JOB 'Compile CICS Program',CLASS=A,MSGCLASS=H,        
//             MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID,TIME=1440
//********************************************************************* 
//*  change CICSPGMN to your program name everywhere                            
//*----->   C CICSPGMN xyz all <--------                                        
//*  set    HLQ      to your high level qualifier                              
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
//****  Sample CICS COBOL Compile JCL                            ******         
//****  Check with your Administrator for                        ******         
//****  JCL suitable to your environment                         ******
//*********************************************************************         
//****  Compile CICS COBOL program                               ******         
//****  After compiling the related maps                         ****** 
//*********************************************************************         
//*  Set Parms for this compile:                                                
//*********************************************************************         
//   SET HLQ=AWS.M2                                                             
//   SET MEMNAME=CICSPGMN                                                       
//*********************************************************************         
//*  Add proclib reference                                                      
//*********************************************************************         
//CCLIBS  JCLLIB ORDER=&HLQ..CARDDEMO.PRC.UTIL                                  
//*********************************************************************         
//*  compile the COBOL code:                                                    
//*********************************************************************         
//CICSCMP      EXEC BUILDONL,MEM=&MEMNAME,HLQ=&HLQ                              
//*********************************************************************         
//****  CICS commands in batch to perform NEWCOPY                ******         
//*********************************************************************         
//NEWCOPY EXEC PGM=SDSF,COND=(4,LT)                                             
//ISFOUT DD SYSOUT=*                                                            
//CMDOUT DD SYSOUT=*                                                            
//ISFIN  DD *                                                                   
 /MODIFY CICSAWSA,'CEMT SET PROG(CICSPGMN) NEWCOPY'                             
/*                                                                              
