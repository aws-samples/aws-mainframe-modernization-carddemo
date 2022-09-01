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
       01  COTRN1AI.
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
           02  TRNIDINL    COMP  PIC  S9(4).
           02  TRNIDINF    PICTURE X.
           02  FILLER REDEFINES TRNIDINF.
             03 TRNIDINA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRNIDINI  PIC X(16).
           02  TRNIDL    COMP  PIC  S9(4).
           02  TRNIDF    PICTURE X.
           02  FILLER REDEFINES TRNIDF.
             03 TRNIDA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRNIDI  PIC X(16).
           02  CARDNUML    COMP  PIC  S9(4).
           02  CARDNUMF    PICTURE X.
           02  FILLER REDEFINES CARDNUMF.
             03 CARDNUMA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CARDNUMI  PIC X(16).
           02  TTYPCDL    COMP  PIC  S9(4).
           02  TTYPCDF    PICTURE X.
           02  FILLER REDEFINES TTYPCDF.
             03 TTYPCDA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TTYPCDI  PIC X(2).
           02  TCATCDL    COMP  PIC  S9(4).
           02  TCATCDF    PICTURE X.
           02  FILLER REDEFINES TCATCDF.
             03 TCATCDA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TCATCDI  PIC X(4).
           02  TRNSRCL    COMP  PIC  S9(4).
           02  TRNSRCF    PICTURE X.
           02  FILLER REDEFINES TRNSRCF.
             03 TRNSRCA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRNSRCI  PIC X(10).
           02  TDESCL    COMP  PIC  S9(4).
           02  TDESCF    PICTURE X.
           02  FILLER REDEFINES TDESCF.
             03 TDESCA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TDESCI  PIC X(60).
           02  TRNAMTL    COMP  PIC  S9(4).
           02  TRNAMTF    PICTURE X.
           02  FILLER REDEFINES TRNAMTF.
             03 TRNAMTA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRNAMTI  PIC X(12).
           02  TORIGDTL    COMP  PIC  S9(4).
           02  TORIGDTF    PICTURE X.
           02  FILLER REDEFINES TORIGDTF.
             03 TORIGDTA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TORIGDTI  PIC X(10).
           02  TPROCDTL    COMP  PIC  S9(4).
           02  TPROCDTF    PICTURE X.
           02  FILLER REDEFINES TPROCDTF.
             03 TPROCDTA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TPROCDTI  PIC X(10).
           02  MIDL    COMP  PIC  S9(4).
           02  MIDF    PICTURE X.
           02  FILLER REDEFINES MIDF.
             03 MIDA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  MIDI  PIC X(9).
           02  MNAMEL    COMP  PIC  S9(4).
           02  MNAMEF    PICTURE X.
           02  FILLER REDEFINES MNAMEF.
             03 MNAMEA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  MNAMEI  PIC X(30).
           02  MCITYL    COMP  PIC  S9(4).
           02  MCITYF    PICTURE X.
           02  FILLER REDEFINES MCITYF.
             03 MCITYA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  MCITYI  PIC X(25).
           02  MZIPL    COMP  PIC  S9(4).
           02  MZIPF    PICTURE X.
           02  FILLER REDEFINES MZIPF.
             03 MZIPA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  MZIPI  PIC X(10).
           02  ERRMSGL    COMP  PIC  S9(4).
           02  ERRMSGF    PICTURE X.
           02  FILLER REDEFINES ERRMSGF.
             03 ERRMSGA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ERRMSGI  PIC X(78).
       01  COTRN1AO REDEFINES COTRN1AI.
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
           02  TRNIDINC    PICTURE X.
           02  TRNIDINP    PICTURE X.
           02  TRNIDINH    PICTURE X.
           02  TRNIDINV    PICTURE X.
           02  TRNIDINO  PIC X(16).
           02  FILLER PICTURE X(3).
           02  TRNIDC    PICTURE X.
           02  TRNIDP    PICTURE X.
           02  TRNIDH    PICTURE X.
           02  TRNIDV    PICTURE X.
           02  TRNIDO  PIC X(16).
           02  FILLER PICTURE X(3).
           02  CARDNUMC    PICTURE X.
           02  CARDNUMP    PICTURE X.
           02  CARDNUMH    PICTURE X.
           02  CARDNUMV    PICTURE X.
           02  CARDNUMO  PIC X(16).
           02  FILLER PICTURE X(3).
           02  TTYPCDC    PICTURE X.
           02  TTYPCDP    PICTURE X.
           02  TTYPCDH    PICTURE X.
           02  TTYPCDV    PICTURE X.
           02  TTYPCDO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  TCATCDC    PICTURE X.
           02  TCATCDP    PICTURE X.
           02  TCATCDH    PICTURE X.
           02  TCATCDV    PICTURE X.
           02  TCATCDO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  TRNSRCC    PICTURE X.
           02  TRNSRCP    PICTURE X.
           02  TRNSRCH    PICTURE X.
           02  TRNSRCV    PICTURE X.
           02  TRNSRCO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  TDESCC    PICTURE X.
           02  TDESCP    PICTURE X.
           02  TDESCH    PICTURE X.
           02  TDESCV    PICTURE X.
           02  TDESCO  PIC X(60).
           02  FILLER PICTURE X(3).
           02  TRNAMTC    PICTURE X.
           02  TRNAMTP    PICTURE X.
           02  TRNAMTH    PICTURE X.
           02  TRNAMTV    PICTURE X.
           02  TRNAMTO  PIC X(12).
           02  FILLER PICTURE X(3).
           02  TORIGDTC    PICTURE X.
           02  TORIGDTP    PICTURE X.
           02  TORIGDTH    PICTURE X.
           02  TORIGDTV    PICTURE X.
           02  TORIGDTO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  TPROCDTC    PICTURE X.
           02  TPROCDTP    PICTURE X.
           02  TPROCDTH    PICTURE X.
           02  TPROCDTV    PICTURE X.
           02  TPROCDTO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  MIDC    PICTURE X.
           02  MIDP    PICTURE X.
           02  MIDH    PICTURE X.
           02  MIDV    PICTURE X.
           02  MIDO  PIC X(9).
           02  FILLER PICTURE X(3).
           02  MNAMEC    PICTURE X.
           02  MNAMEP    PICTURE X.
           02  MNAMEH    PICTURE X.
           02  MNAMEV    PICTURE X.
           02  MNAMEO  PIC X(30).
           02  FILLER PICTURE X(3).
           02  MCITYC    PICTURE X.
           02  MCITYP    PICTURE X.
           02  MCITYH    PICTURE X.
           02  MCITYV    PICTURE X.
           02  MCITYO  PIC X(25).
           02  FILLER PICTURE X(3).
           02  MZIPC    PICTURE X.
           02  MZIPP    PICTURE X.
           02  MZIPH    PICTURE X.
           02  MZIPV    PICTURE X.
           02  MZIPO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ERRMSGC    PICTURE X.
           02  ERRMSGP    PICTURE X.
           02  ERRMSGH    PICTURE X.
           02  ERRMSGV    PICTURE X.
           02  ERRMSGO  PIC X(78).
