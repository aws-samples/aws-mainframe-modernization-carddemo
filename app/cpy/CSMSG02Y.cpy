000700*****************************************************************
000800* CABENDD.CPY                                                   *
000900*---------------------------------------------------------------*
001000* Work areas for abend routine                                  *
      ******************************************************************
      * Copyright Amazon.com, Inc. or its affiliates.                   
      * All Rights Reserved.                                            
      *                                                                 
      * Licensed under the Apache License, Version 2.0 (the "License"). 
      * You may not use this file except in compliance with the License.
      * You may obtain a copy of the License at                         
      *                                                                 
      *    http://www.apache.org/licenses/LICENSE-2.0                   
      *                                                                 
      * Unless required by applicable law or agreed to in writing,      
      * software distributed under the License is distributed on an     
      * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    
      * either express or implied. See the License for the specific     
      * language governing permissions and limitations under the License
      ****************************************************************** 
001200 01  ABEND-DATA.
001300   05  ABEND-CODE                            PIC X(4)
001400       VALUE SPACES.
001500   05  ABEND-CULPRIT                         PIC X(8)
001600       VALUE SPACES.
001700   05  ABEND-REASON                          PIC X(50)
001800       VALUE SPACES.
001900   05  ABEND-MSG                             PIC X(72)
002000       VALUE SPACES.



      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:15:58 CDT
      *
