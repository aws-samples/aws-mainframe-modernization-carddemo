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
       01  COUSR3AI.
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
           02  USRIDINL    COMP  PIC  S9(4).
           02  USRIDINF    PICTURE X.
           02  FILLER REDEFINES USRIDINF.
             03 USRIDINA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  USRIDINI  PIC X(8).
           02  FNAMEL    COMP  PIC  S9(4).
           02  FNAMEF    PICTURE X.
           02  FILLER REDEFINES FNAMEF.
             03 FNAMEA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  FNAMEI  PIC X(20).
           02  LNAMEL    COMP  PIC  S9(4).
           02  LNAMEF    PICTURE X.
           02  FILLER REDEFINES LNAMEF.
             03 LNAMEA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  LNAMEI  PIC X(20).
           02  USRTYPEL    COMP  PIC  S9(4).
           02  USRTYPEF    PICTURE X.
           02  FILLER REDEFINES USRTYPEF.
             03 USRTYPEA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  USRTYPEI  PIC X(1).
           02  ERRMSGL    COMP  PIC  S9(4).
           02  ERRMSGF    PICTURE X.
           02  FILLER REDEFINES ERRMSGF.
             03 ERRMSGA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ERRMSGI  PIC X(78).
       01  COUSR3AO REDEFINES COUSR3AI.
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
           02  USRIDINC    PICTURE X.
           02  USRIDINP    PICTURE X.
           02  USRIDINH    PICTURE X.
           02  USRIDINV    PICTURE X.
           02  USRIDINO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  FNAMEC    PICTURE X.
           02  FNAMEP    PICTURE X.
           02  FNAMEH    PICTURE X.
           02  FNAMEV    PICTURE X.
           02  FNAMEO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  LNAMEC    PICTURE X.
           02  LNAMEP    PICTURE X.
           02  LNAMEH    PICTURE X.
           02  LNAMEV    PICTURE X.
           02  LNAMEO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  USRTYPEC    PICTURE X.
           02  USRTYPEP    PICTURE X.
           02  USRTYPEH    PICTURE X.
           02  USRTYPEV    PICTURE X.
           02  USRTYPEO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ERRMSGC    PICTURE X.
           02  ERRMSGP    PICTURE X.
           02  ERRMSGH    PICTURE X.
           02  ERRMSGV    PICTURE X.
           02  ERRMSGO  PIC X(78).
