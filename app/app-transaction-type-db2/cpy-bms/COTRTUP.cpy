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
       01  CTRTUPAI.
           02  FILLER PIC X(12).
           02  TRNNAMEL    COMP  PIC  S9(4).
           02  TRNNAMEF    PICTURE X.
           02  FILLER REDEFINES TRNNAMEF.
             03 TRNNAMEA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRNNAMEI  PIC X(4).
           02  TITLE01L    COMP  PIC  S9(4).
           02  TITLE01F    PICTURE X.
           02  FILLER REDEFINES TITLE01F.
             03 TITLE01A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TITLE01I  PIC X(40).
           02  CURDATEL    COMP  PIC  S9(4).
           02  CURDATEF    PICTURE X.
           02  FILLER REDEFINES CURDATEF.
             03 CURDATEA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CURDATEI  PIC X(8).
           02  PGMNAMEL    COMP  PIC  S9(4).
           02  PGMNAMEF    PICTURE X.
           02  FILLER REDEFINES PGMNAMEF.
             03 PGMNAMEA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  PGMNAMEI  PIC X(8).
           02  TITLE02L    COMP  PIC  S9(4).
           02  TITLE02F    PICTURE X.
           02  FILLER REDEFINES TITLE02F.
             03 TITLE02A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TITLE02I  PIC X(40).
           02  CURTIMEL    COMP  PIC  S9(4).
           02  CURTIMEF    PICTURE X.
           02  FILLER REDEFINES CURTIMEF.
             03 CURTIMEA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CURTIMEI  PIC X(8).
           02  TRTYPCDL    COMP  PIC  S9(4).
           02  TRTYPCDF    PICTURE X.
           02  FILLER REDEFINES TRTYPCDF.
             03 TRTYPCDA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTYPCDI  PIC X(2).
           02  TRTYDSCL    COMP  PIC  S9(4).
           02  TRTYDSCF    PICTURE X.
           02  FILLER REDEFINES TRTYDSCF.
             03 TRTYDSCA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTYDSCI  PIC X(50).
           02  INFOMSGL    COMP  PIC  S9(4).
           02  INFOMSGF    PICTURE X.
           02  FILLER REDEFINES INFOMSGF.
             03 INFOMSGA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  INFOMSGI  PIC X(45).
           02  ERRMSGL    COMP  PIC  S9(4).
           02  ERRMSGF    PICTURE X.
           02  FILLER REDEFINES ERRMSGF.
             03 ERRMSGA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ERRMSGI  PIC X(78).
           02  FKEYSL    COMP  PIC  S9(4).
           02  FKEYSF    PICTURE X.
           02  FILLER REDEFINES FKEYSF.
             03 FKEYSA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  FKEYSI  PIC X(21).
           02  FKEY04L    COMP  PIC  S9(4).
           02  FKEY04F    PICTURE X.
           02  FILLER REDEFINES FKEY04F.
             03 FKEY04A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  FKEY04I  PIC X(9).
           02  FKEY05L    COMP  PIC  S9(4).
           02  FKEY05F    PICTURE X.
           02  FILLER REDEFINES FKEY05F.
             03 FKEY05A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  FKEY05I  PIC X(8).
           02  FKEY06L    COMP  PIC  S9(4).
           02  FKEY06F    PICTURE X.
           02  FILLER REDEFINES FKEY06F.
             03 FKEY06A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  FKEY06I  PIC X(6).
           02  FKEY12L    COMP  PIC  S9(4).
           02  FKEY12F    PICTURE X.
           02  FILLER REDEFINES FKEY12F.
             03 FKEY12A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  FKEY12I  PIC X(10).
       01  CTRTUPAO REDEFINES CTRTUPAI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  TRNNAMEC    PICTURE X.
           02  TRNNAMEP    PICTURE X.
           02  TRNNAMEH    PICTURE X.
           02  TRNNAMEV    PICTURE X.
           02  TRNNAMEO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  TITLE01C    PICTURE X.
           02  TITLE01P    PICTURE X.
           02  TITLE01H    PICTURE X.
           02  TITLE01V    PICTURE X.
           02  TITLE01O  PIC X(40).
           02  FILLER PICTURE X(3).
           02  CURDATEC    PICTURE X.
           02  CURDATEP    PICTURE X.
           02  CURDATEH    PICTURE X.
           02  CURDATEV    PICTURE X.
           02  CURDATEO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  PGMNAMEC    PICTURE X.
           02  PGMNAMEP    PICTURE X.
           02  PGMNAMEH    PICTURE X.
           02  PGMNAMEV    PICTURE X.
           02  PGMNAMEO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  TITLE02C    PICTURE X.
           02  TITLE02P    PICTURE X.
           02  TITLE02H    PICTURE X.
           02  TITLE02V    PICTURE X.
           02  TITLE02O  PIC X(40).
           02  FILLER PICTURE X(3).
           02  CURTIMEC    PICTURE X.
           02  CURTIMEP    PICTURE X.
           02  CURTIMEH    PICTURE X.
           02  CURTIMEV    PICTURE X.
           02  CURTIMEO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  TRTYPCDC    PICTURE X.
           02  TRTYPCDP    PICTURE X.
           02  TRTYPCDH    PICTURE X.
           02  TRTYPCDV    PICTURE X.
           02  TRTYPCDO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  TRTYDSCC    PICTURE X.
           02  TRTYDSCP    PICTURE X.
           02  TRTYDSCH    PICTURE X.
           02  TRTYDSCV    PICTURE X.
           02  TRTYDSCO  PIC X(50).
           02  FILLER PICTURE X(3).
           02  INFOMSGC    PICTURE X.
           02  INFOMSGP    PICTURE X.
           02  INFOMSGH    PICTURE X.
           02  INFOMSGV    PICTURE X.
           02  INFOMSGO  PIC X(45).
           02  FILLER PICTURE X(3).
           02  ERRMSGC    PICTURE X.
           02  ERRMSGP    PICTURE X.
           02  ERRMSGH    PICTURE X.
           02  ERRMSGV    PICTURE X.
           02  ERRMSGO  PIC X(78).
           02  FILLER PICTURE X(3).
           02  FKEYSC    PICTURE X.
           02  FKEYSP    PICTURE X.
           02  FKEYSH    PICTURE X.
           02  FKEYSV    PICTURE X.
           02  FKEYSO  PIC X(21).
           02  FILLER PICTURE X(3).
           02  FKEY04C    PICTURE X.
           02  FKEY04P    PICTURE X.
           02  FKEY04H    PICTURE X.
           02  FKEY04V    PICTURE X.
           02  FKEY04O  PIC X(9).
           02  FILLER PICTURE X(3).
           02  FKEY05C    PICTURE X.
           02  FKEY05P    PICTURE X.
           02  FKEY05H    PICTURE X.
           02  FKEY05V    PICTURE X.
           02  FKEY05O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  FKEY06C    PICTURE X.
           02  FKEY06P    PICTURE X.
           02  FKEY06H    PICTURE X.
           02  FKEY06V    PICTURE X.
           02  FKEY06O  PIC X(6).
           02  FILLER PICTURE X(3).
           02  FKEY12C    PICTURE X.
           02  FKEY12P    PICTURE X.
           02  FKEY12H    PICTURE X.
           02  FKEY12V    PICTURE X.
           02  FKEY12O  PIC X(10).
