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
      *    Set (TESTVAR1) to red if in error and * if blankACSHLIM
           IF (FLG-(TESTVAR1)-NOT-OK                                 
           OR  FLG-(TESTVAR1)-BLANK)                                    
           AND CDEMO-PGM-REENTER                                        
               MOVE DFHRED             TO 
                    (SCRNVAR2)C OF (MAPNAME3)O
               IF  FLG-(TESTVAR1)-BLANK                                 
                   MOVE '*'            TO 
                    (SCRNVAR2)O OF (MAPNAME3)O
               END-IF                                                   
           END-IF 
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:15:58 CDT
      *
