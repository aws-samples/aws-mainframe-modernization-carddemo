//SORTTEST JOB 'SORT STEP TESTING',CLASS=A,MSGCLASS=0,MSGLEVEL=(1,1),           
//             REGION=0M,NOTIFY=&SYSUID,TIME=1440                      
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
//STEP05R  EXEC PGM=SORT                                                  
//SORTIN   DD DISP=SHR,                                                 
//         DSN=AWS.M2.CARDDEMO.TRANSACT.BKUP(0)                                 
//SYMNAMES DD *
TRAN-CARD-NUM,263,16,ZD
TRAN-PROC-DT,305,10,CH               
PARM-DATE,C'2022-06-02'                              
//SYSIN    DD *                                                         
 SORT FIELDS=(TRAN-CARD-NUM,A) 
 INCLUDE COND=(TRAN-PROC-DT,EQ,PARM-DATE)                                       
/*                                                                      
//SYSOUT   DD SYSOUT=*                                                  
//SORTOUT  DD DISP=(NEW,CATLG,DELETE),
//         UNIT=SYSDA,
//         DCB=(*.SORTIN),
//         SPACE=(CYL,(1,1),RLSE),
//         DSN=AWS.M2.CARDDEMO.TRANSACT.DALY(+1)
//*
//* Ver: CardDemo_v1.0-57-g40b7caa-110 Date: 2022-08-19 18:00:43 CDT
//*
