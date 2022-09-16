//TRANIDX JOB 'Define AIX on Transaction Master',CLASS=A,MSGCLASS=0,         
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
//*-------------------------------------------------------------------*         
//* CREATE ALTERNATE INDEX ON PROCESSED TIMESTAMP                               
//*-------------------------------------------------------------------*         
//STEP20  EXEC PGM=IDCAMS                                                       
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  *                                                                
   DEFINE ALTERNATEINDEX (NAME(AWS.M2.CARDDEMO.TRANSACT.VSAM.AIX)-              
   RELATE(AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS)                    -              
   KEYS(26 304)                                                  -             
   NONUNIQUEKEY                                                  -              
   UPGRADE                                                       -              
   RECORDSIZE(350,350)                                           -              
   VOLUMES(AWSHJ1)                                               -              
   CYLINDERS(5,1))                                               -              
   DATA (NAME(AWS.M2.CARDDEMO.TRANSACT.VSAM.AIX.DATA))           -              
   INDEX (NAME(AWS.M2.CARDDEMO.TRANSACT.VSAM.AIX.INDEX))                        
/*                                                                              
//*-------------------------------------------------------------------*         
//* DEFINE PATH IS USED TO RELATE THE ALTERNATE INDEX TO BASE CLUSTER           
//*-------------------------------------------------------------------*         
//STEP25  EXEC PGM=IDCAMS                                                       
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  *                                                                
  DEFINE PATH                                           -                       
   (NAME(AWS.M2.CARDDEMO.TRANSACT.VSAM.AIX.PATH)        -                       
    PATHENTRY(AWS.M2.CARDDEMO.TRANSACT.VSAM.AIX))                               
/*                                                                              
//*------------------------------------------------------------------           
//* BUILD ALTERNATE INDEX CLUSTER                                               
//*-------------------------------------------------------------------*         
//STEP30  EXEC PGM=IDCAMS                                                       
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  *                                                                
   BLDINDEX                                                      -              
   INDATASET(AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS)                 -              
   OUTDATASET(AWS.M2.CARDDEMO.TRANSACT.VSAM.AIX)                                
/*  
//*
//* Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:23:08 CDT
//*
