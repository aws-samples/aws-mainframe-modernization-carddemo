//REPROC PROC                                                                   
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
//* Unload the processed transaction file                                       
//* *******************************************************************         
//STEP01R EXEC PROC=REPROC,                                                     
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
//* *******************************************************************         
//* Filter the transactions for a the parm date and sort by card num            
//* *******************************************************************         
//STEP05R  EXEC PGM=SORT                                                        
//SORTIN   DD DISP=SHR,                                                         
//         DSN=AWS.M2.CARDDEMO.TRANSACT.BKUP(+1)                                
//SYMNAMES DD *                                                                 
TRAN-CARD-NUM,263,16,ZD                                                         
TRAN-PROC-DT,305,10,CH                                                          
PARM-START-DATE,C'2022-01-01'                                      //Date       
PARM-END-DATE,C'2022-07-06'                                        //Date       
//SYSIN    DD *                                                                 
 SORT FIELDS=(TRAN-CARD-NUM,A)                                                  
 INCLUDE COND=(TRAN-PROC-DT,GE,PARM-START-DATE,AND,                             
         TRAN-PROC-DT,LE,PARM-END-DATE)                                         
/*                                                                              
//SYSOUT   DD SYSOUT=*                                                          
//SORTOUT  DD DISP=(NEW,CATLG,DELETE),                                          
//         UNIT=SYSDA,                                                          
//         DCB=(*.SORTIN),                                                      
//         SPACE=(CYL,(1,1),RLSE),                                              
//         DSN=AWS.M2.CARDDEMO.TRANSACT.DALY(+1)                                
//* *******************************************************************         
//* Produce a formatted report for processed transactions                       
//* *******************************************************************         
//STEP10R EXEC PGM=CBTRN03C                                                     
//STEPLIB  DD DISP=SHR,                                                         
//         DSN=AWS.M2.CARDDEMO.LOADLIB                                          
//SYSOUT   DD SYSOUT=*                                                          
//SYSPRINT DD SYSOUT=*                                                          
//* Input files                                                                 
//TRANFILE DD DISP=SHR,                                                         
//         DSN=AWS.M2.CARDDEMO.TRANSACT.DALY(+1)                                
//CARDXREF DD DISP=SHR,                                                         
//         DSN=AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS                               
//TRANTYPE DD DISP=SHR,                                                         
//         DSN=AWS.M2.CARDDEMO.TRANTYPE.VSAM.KSDS                               
//TRANCATG DD DISP=SHR,                                                         
//         DSN=AWS.M2.CARDDEMO.TRANCATG.VSAM.KSDS                               
//DATEPARM DD DISP=SHR,                                                         
//         DSN=AWS.M2.CARDDEMO.DATEPARM                                         
//* Output files                                                                
//TRANREPT DD DISP=(NEW,CATLG,DELETE),                                          
//         UNIT=SYSDA,                                                          
//         DCB=(LRECL=133,RECFM=FB,BLKSIZE=0),                                  
//         SPACE=(CYL,(1,1),RLSE),                                              
//         DSN=AWS.M2.CARDDEMO.TRANREPT(+1)                                     
// PEND                                                                         
//*
//* Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:27:38 CDT
//*
