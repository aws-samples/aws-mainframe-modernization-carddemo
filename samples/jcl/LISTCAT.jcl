//LISTFILS JOB 'CARDDEMO',CLASS=A,MSGCLASS=0,NOTIFY=&SYSUID
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
//*******************************************************************
//* List catalog for all the files in CARDDEMO application           
//*******************************************************************
//STEP01 EXEC PGM=IEFBR14
//DD1      DD   DISP=(MOD,DELETE,DELETE),
//         UNIT=SYSDA,
//         DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0),
//         SPACE=(TRK,(1,1)),
//         DSN=AWS.M2.CARDDEMO.LISTCAT
//STEP05 EXEC PGM=IDCAMS,COND=(0,NE)
//SYSPRINT DD   DISP=(NEW,CATLG,DELETE),
//         UNIT=SYSDA,
//         DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0),
//         SPACE=(TRK,(1,1)),
//         DSN=AWS.M2.CARDDEMO.LISTCAT
//SYSIN    DD   *
   LISTCAT LEVEL(AWS.M2.CARDDEMO)  -
           ALL
/*
//*
//* Ver: CardDemo_v1.0-57-g40b7caa-110 Date: 2022-08-19 18:00:43 CDT
//*
