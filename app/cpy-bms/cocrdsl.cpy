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
       01  CCRDSLAI.
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
           02  ACCTSIDL    COMP  PIC  S9(4).
           02  ACCTSIDF    PICTURE X.
           02  FILLER REDEFINES ACCTSIDF.
             03 ACCTSIDA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACCTSIDI  PIC X(11).
           02  CARDSIDL    COMP  PIC  S9(4).
           02  CARDSIDF    PICTURE X.
           02  FILLER REDEFINES CARDSIDF.
             03 CARDSIDA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CARDSIDI  PIC X(16).
           02  CRDNAMEL    COMP  PIC  S9(4).
           02  CRDNAMEF    PICTURE X.
           02  FILLER REDEFINES CRDNAMEF.
             03 CRDNAMEA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDNAMEI  PIC X(50).
           02  CRDSTCDL    COMP  PIC  S9(4).
           02  CRDSTCDF    PICTURE X.
           02  FILLER REDEFINES CRDSTCDF.
             03 CRDSTCDA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDSTCDI  PIC X(1).
           02  EXPMONL    COMP  PIC  S9(4).
           02  EXPMONF    PICTURE X.
           02  FILLER REDEFINES EXPMONF.
             03 EXPMONA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  EXPMONI  PIC X(2).
           02  EXPYEARL    COMP  PIC  S9(4).
           02  EXPYEARF    PICTURE X.
           02  FILLER REDEFINES EXPYEARF.
             03 EXPYEARA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  EXPYEARI  PIC X(4).
           02  INFOMSGL    COMP  PIC  S9(4).
           02  INFOMSGF    PICTURE X.
           02  FILLER REDEFINES INFOMSGF.
             03 INFOMSGA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  INFOMSGI  PIC X(40).
           02  ERRMSGL    COMP  PIC  S9(4).
           02  ERRMSGF    PICTURE X.
           02  FILLER REDEFINES ERRMSGF.
             03 ERRMSGA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ERRMSGI  PIC X(80).
           02  FKEYSL    COMP  PIC  S9(4).
           02  FKEYSF    PICTURE X.
           02  FILLER REDEFINES FKEYSF.
             03 FKEYSA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  FKEYSI  PIC X(75).
       01  CCRDSLAO REDEFINES CCRDSLAI.
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
           02  ACCTSIDC    PICTURE X.
           02  ACCTSIDP    PICTURE X.
           02  ACCTSIDH    PICTURE X.
           02  ACCTSIDV    PICTURE X.
           02  ACCTSIDO  PIC X(11).
           02  FILLER PICTURE X(3).
           02  CARDSIDC    PICTURE X.
           02  CARDSIDP    PICTURE X.
           02  CARDSIDH    PICTURE X.
           02  CARDSIDV    PICTURE X.
           02  CARDSIDO  PIC X(16).
           02  FILLER PICTURE X(3).
           02  CRDNAMEC    PICTURE X.
           02  CRDNAMEP    PICTURE X.
           02  CRDNAMEH    PICTURE X.
           02  CRDNAMEV    PICTURE X.
           02  CRDNAMEO  PIC X(50).
           02  FILLER PICTURE X(3).
           02  CRDSTCDC    PICTURE X.
           02  CRDSTCDP    PICTURE X.
           02  CRDSTCDH    PICTURE X.
           02  CRDSTCDV    PICTURE X.
           02  CRDSTCDO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  EXPMONC    PICTURE X.
           02  EXPMONP    PICTURE X.
           02  EXPMONH    PICTURE X.
           02  EXPMONV    PICTURE X.
           02  EXPMONO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  EXPYEARC    PICTURE X.
           02  EXPYEARP    PICTURE X.
           02  EXPYEARH    PICTURE X.
           02  EXPYEARV    PICTURE X.
           02  EXPYEARO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  INFOMSGC    PICTURE X.
           02  INFOMSGP    PICTURE X.
           02  INFOMSGH    PICTURE X.
           02  INFOMSGV    PICTURE X.
           02  INFOMSGO  PIC X(40).
           02  FILLER PICTURE X(3).
           02  ERRMSGC    PICTURE X.
           02  ERRMSGP    PICTURE X.
           02  ERRMSGH    PICTURE X.
           02  ERRMSGV    PICTURE X.
           02  ERRMSGO  PIC X(80).
           02  FILLER PICTURE X(3).
           02  FKEYSC    PICTURE X.
           02  FKEYSP    PICTURE X.
           02  FKEYSH    PICTURE X.
           02  FKEYSV    PICTURE X.
           02  FKEYSO  PIC X(75).
