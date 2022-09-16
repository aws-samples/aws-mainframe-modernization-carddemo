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
       01  COMEN1AI.
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
           02  OPTN001L    COMP  PIC  S9(4).
           02  OPTN001F    PICTURE X.
           02  FILLER REDEFINES OPTN001F.
             03 OPTN001A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  OPTN001I  PIC X(40).
           02  OPTN002L    COMP  PIC  S9(4).
           02  OPTN002F    PICTURE X.
           02  FILLER REDEFINES OPTN002F.
             03 OPTN002A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  OPTN002I  PIC X(40).
           02  OPTN003L    COMP  PIC  S9(4).
           02  OPTN003F    PICTURE X.
           02  FILLER REDEFINES OPTN003F.
             03 OPTN003A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  OPTN003I  PIC X(40).
           02  OPTN004L    COMP  PIC  S9(4).
           02  OPTN004F    PICTURE X.
           02  FILLER REDEFINES OPTN004F.
             03 OPTN004A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  OPTN004I  PIC X(40).
           02  OPTN005L    COMP  PIC  S9(4).
           02  OPTN005F    PICTURE X.
           02  FILLER REDEFINES OPTN005F.
             03 OPTN005A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  OPTN005I  PIC X(40).
           02  OPTN006L    COMP  PIC  S9(4).
           02  OPTN006F    PICTURE X.
           02  FILLER REDEFINES OPTN006F.
             03 OPTN006A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  OPTN006I  PIC X(40).
           02  OPTN007L    COMP  PIC  S9(4).
           02  OPTN007F    PICTURE X.
           02  FILLER REDEFINES OPTN007F.
             03 OPTN007A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  OPTN007I  PIC X(40).
           02  OPTN008L    COMP  PIC  S9(4).
           02  OPTN008F    PICTURE X.
           02  FILLER REDEFINES OPTN008F.
             03 OPTN008A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  OPTN008I  PIC X(40).
           02  OPTN009L    COMP  PIC  S9(4).
           02  OPTN009F    PICTURE X.
           02  FILLER REDEFINES OPTN009F.
             03 OPTN009A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  OPTN009I  PIC X(40).
           02  OPTN010L    COMP  PIC  S9(4).
           02  OPTN010F    PICTURE X.
           02  FILLER REDEFINES OPTN010F.
             03 OPTN010A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  OPTN010I  PIC X(40).
           02  OPTN011L    COMP  PIC  S9(4).
           02  OPTN011F    PICTURE X.
           02  FILLER REDEFINES OPTN011F.
             03 OPTN011A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  OPTN011I  PIC X(40).
           02  OPTN012L    COMP  PIC  S9(4).
           02  OPTN012F    PICTURE X.
           02  FILLER REDEFINES OPTN012F.
             03 OPTN012A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  OPTN012I  PIC X(40).
           02  OPTIONL    COMP  PIC  S9(4).
           02  OPTIONF    PICTURE X.
           02  FILLER REDEFINES OPTIONF.
             03 OPTIONA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  OPTIONI  PIC X(2).
           02  ERRMSGL    COMP  PIC  S9(4).
           02  ERRMSGF    PICTURE X.
           02  FILLER REDEFINES ERRMSGF.
             03 ERRMSGA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ERRMSGI  PIC X(78).
       01  COMEN1AO REDEFINES COMEN1AI.
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
           02  OPTN001C    PICTURE X.
           02  OPTN001P    PICTURE X.
           02  OPTN001H    PICTURE X.
           02  OPTN001V    PICTURE X.
           02  OPTN001O  PIC X(40).
           02  FILLER PICTURE X(3).
           02  OPTN002C    PICTURE X.
           02  OPTN002P    PICTURE X.
           02  OPTN002H    PICTURE X.
           02  OPTN002V    PICTURE X.
           02  OPTN002O  PIC X(40).
           02  FILLER PICTURE X(3).
           02  OPTN003C    PICTURE X.
           02  OPTN003P    PICTURE X.
           02  OPTN003H    PICTURE X.
           02  OPTN003V    PICTURE X.
           02  OPTN003O  PIC X(40).
           02  FILLER PICTURE X(3).
           02  OPTN004C    PICTURE X.
           02  OPTN004P    PICTURE X.
           02  OPTN004H    PICTURE X.
           02  OPTN004V    PICTURE X.
           02  OPTN004O  PIC X(40).
           02  FILLER PICTURE X(3).
           02  OPTN005C    PICTURE X.
           02  OPTN005P    PICTURE X.
           02  OPTN005H    PICTURE X.
           02  OPTN005V    PICTURE X.
           02  OPTN005O  PIC X(40).
           02  FILLER PICTURE X(3).
           02  OPTN006C    PICTURE X.
           02  OPTN006P    PICTURE X.
           02  OPTN006H    PICTURE X.
           02  OPTN006V    PICTURE X.
           02  OPTN006O  PIC X(40).
           02  FILLER PICTURE X(3).
           02  OPTN007C    PICTURE X.
           02  OPTN007P    PICTURE X.
           02  OPTN007H    PICTURE X.
           02  OPTN007V    PICTURE X.
           02  OPTN007O  PIC X(40).
           02  FILLER PICTURE X(3).
           02  OPTN008C    PICTURE X.
           02  OPTN008P    PICTURE X.
           02  OPTN008H    PICTURE X.
           02  OPTN008V    PICTURE X.
           02  OPTN008O  PIC X(40).
           02  FILLER PICTURE X(3).
           02  OPTN009C    PICTURE X.
           02  OPTN009P    PICTURE X.
           02  OPTN009H    PICTURE X.
           02  OPTN009V    PICTURE X.
           02  OPTN009O  PIC X(40).
           02  FILLER PICTURE X(3).
           02  OPTN010C    PICTURE X.
           02  OPTN010P    PICTURE X.
           02  OPTN010H    PICTURE X.
           02  OPTN010V    PICTURE X.
           02  OPTN010O  PIC X(40).
           02  FILLER PICTURE X(3).
           02  OPTN011C    PICTURE X.
           02  OPTN011P    PICTURE X.
           02  OPTN011H    PICTURE X.
           02  OPTN011V    PICTURE X.
           02  OPTN011O  PIC X(40).
           02  FILLER PICTURE X(3).
           02  OPTN012C    PICTURE X.
           02  OPTN012P    PICTURE X.
           02  OPTN012H    PICTURE X.
           02  OPTN012V    PICTURE X.
           02  OPTN012O  PIC X(40).
           02  FILLER PICTURE X(3).
           02  OPTIONC    PICTURE X.
           02  OPTIONP    PICTURE X.
           02  OPTIONH    PICTURE X.
           02  OPTIONV    PICTURE X.
           02  OPTIONO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  ERRMSGC    PICTURE X.
           02  ERRMSGP    PICTURE X.
           02  ERRMSGH    PICTURE X.
           02  ERRMSGV    PICTURE X.
           02  ERRMSGO  PIC X(78).
