//FTPJCLS  JOB 'FTP JCL',CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),             JOB01598
//             REGION=0M,NOTIFY=&SYSUID,TIME=1440
//*
//*
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
//* FTP JOB TO RECEIVE A FILE
//* MY.FTP.SERVER.COM IS THE FTP SERVER NAME(SUBJECT TO CHANGE FOR SITE)
//* USER ID - EITHER SCHEDULER ID OR TSO USER ID
//* PASSWORD OF THE USER
//* FOLDER TO FTP THE MAINFRAME FILE
//* PUT COMMAND FOR SENDING MAINFRAME FILE TO TXT
//* CHANGE THE PUT COMMAND TO GET TO RECEIVE FILE INTO MAINFRAME
//*********************************************************************
//STEP1 EXEC PGM=FTP,REGION=2048K
//*          PARM='10.81.148.4 (EXIT TIMEOUT 20'
//SYSIN DD *
 172.31.21.124
 carddemousr
 ftpdemo1
 ASCII
 pwd
 dir
 cd /ftpfolder
 PUT 'AWS.M2.CARDEMO.FTP.TEST' welcome.txt
 QUIT
//*
