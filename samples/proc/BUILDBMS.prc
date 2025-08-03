//*********************************************************************
//*  THIS PROC IS USED TO COMPILE BMS MAPS
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
//****  Sample BMS Compile PROC                                  ******         
//****  Check with your Administrator for                        ******         
//****  JCL suitable to your environment                         ******
//*********************************************************************
//BUILDBMS PROC MAPNAME=,                                                       
//            HLQ=,                                                             
//            SRCCODE=&HLQ..CARDDEMO.BMS,                                       
//            LOADLIB=&HLQ..CARDDEMO.LOADLIB,                                   
//            CPYBKS=&HLQ..CARDDEMO.CPY,                                        
//            CICSMAC=OEM.CICSTS.V05R06M0.CICS.SDFHMAC,                         
//            LISTING=&HLQ..CARDDEMO.LST                                        
//*  ---------------------------                                                
//* PROCEDURE TO COMPILE BMS MAP                                                
//*  ---------------------------                                                
//*      PRINT BMS MAP DATA                                                     
//*  ---------------------------                                                
//*                                                                             
//PRINT    EXEC PGM=IEBGENER                                                    
//SYSPRINT DD SYSOUT=*                                                          
//SYSUT2   DD DSN=&&TEMPM,UNIT=3390,DISP=(,PASS),                               
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=400),                              
//            SPACE=(400,(50,50))                                               
//SYSIN    DD DUMMY                                                             
//SYSUT1   DD DSN=&SRCCODE(&MAPNAME),                                           
//            DISP=SHR                                                          
//*                                                                             
//*  ---------------------------                                                
//*         ASSEMBLE MAP                                                        
//*  ---------------------------                                                
//*                                                                             
//MAP      EXEC PGM=ASMA90,PARM='SYSPARM(MAP),DECK,NOLOAD'                      
//SYSPRINT DD SYSOUT=*                                                          
//*YSPRINT DD DSN=&LISTING(&MAPNAME),DISP=SHR                                   
//SYSLIB   DD DSN=&CICSMAC,DISP=SHR                                             
//         DD DSN=SYS1.MACLIB,DISP=SHR                                          
//         DD DSN=CEE.SCEEMAC,DISP=SHR                                          
//SYSUT1   DD UNIT=3390,SPACE=(TRK,(15,15))                                     
//SYSUT2   DD UNIT=3390,SPACE=(TRK,(15,15))                                     
//SYSUT3   DD UNIT=3390,SPACE=(TRK,(15,15))                                     
//SYSPUNCH DD DSN=&&MAP,DISP=(,PASS),UNIT=3390,                                 
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=400),                              
//            SPACE=(400,(50,50))                                               
//SYSIN    DD DSN=&&TEMPM,DISP=(OLD,PASS)                                       
//SYSLIN   DD DUMMY                                                             
//*                                                                             
//*  ---------------------------                                                
//*         LINK-EDIT STEP                                                      
//*  ---------------------------                                                
//*                                                                             
//LKED     EXEC PGM=HEWL,PARM='LIST,LET,XREF'                                   
//SYSPRINT DD SYSOUT=*                                                          
//SYSLMOD  DD DSN=&LOADLIB(&MAPNAME),DISP=SHR                                   
//SYSUT1   DD UNIT=3390,SPACE=(1024,(20,20))                                    
//SYSLIN   DD DSN=&&MAP,DISP=(OLD,DELETE)                                       
//*                                                                             
//*  ---------------------------                                                
//*        DSECT GENERATION                                                     
//*  ---------------------------                                                
//*                                                                             
//DSECT    EXEC PGM=ASMA90,PARM='SYSPARM(DSECT),DECK,NOLOAD'                    
//*SECT    EXEC PGM=ASMA90,PARM='SYSPARM(DSECT),DECK'                           
//*YSPRINT DD SYSOUT=*                                                          
//SYSPRINT DD DSN=&LISTING(&MAPNAME),DISP=SHR                                   
//SYSLIB   DD DSN=&CICSMAC,DISP=SHR                                             
//         DD DSN=SYS1.MACLIB,DISP=SHR                                          
//         DD DSN=CEE.SCEEMAC,DISP=SHR                                          
//SYSUT1   DD UNIT=3390,SPACE=(TRK,(15,15))                                     
//SYSUT2   DD UNIT=3390,SPACE=(TRK,(15,15))                                     
//SYSUT3   DD UNIT=3390,SPACE=(TRK,(15,15))                                     
//SYSPUNCH DD DSN=&CPYBKS(&MAPNAME),DISP=OLD,                                   
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=400),                              
//            SPACE=(400,(50,50)),UNIT=3390                                     
//SYSIN    DD DSN=&&TEMPM,DISP=(OLD,PASS)                                       
//SYSLIN   DD DUMMY                                                             
//*  *********************************                                          
//*      REPLICATE LISTING ON SYSOUT                                            
//*  *********************************                                          
//DISPLIST EXEC PGM=IEBGENER,REGION=0M                                          
//SYSPRINT DD SYSOUT=*                                                          
//SYSUT1   DD DSN=&LISTING(&MAPNAME),DISP=SHR                                   
//SYSUT2   DD SYSOUT=*                                                          
//SYSPRINT DD SYSOUT=*                                                          
//SYSIN    DD DUMMY                                                             
//*                                                                             
//DELTEMP EXEC PGM=IEFBR14                                                      
//TEMPM   DD DSN=&&TEMPM,DISP=(OLD,DELETE)                                      
//*                                                                             
