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
       01  CACTUPAI.
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
           02  ACSTTUSL    COMP  PIC  S9(4).
           02  ACSTTUSF    PICTURE X.
           02  FILLER REDEFINES ACSTTUSF.
             03 ACSTTUSA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSTTUSI  PIC X(1).
           02  OPNYEARL    COMP  PIC  S9(4).
           02  OPNYEARF    PICTURE X.
           02  FILLER REDEFINES OPNYEARF.
             03 OPNYEARA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  OPNYEARI  PIC X(4).
           02  OPNMONL    COMP  PIC  S9(4).
           02  OPNMONF    PICTURE X.
           02  FILLER REDEFINES OPNMONF.
             03 OPNMONA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  OPNMONI  PIC X(2).
           02  OPNDAYL    COMP  PIC  S9(4).
           02  OPNDAYF    PICTURE X.
           02  FILLER REDEFINES OPNDAYF.
             03 OPNDAYA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  OPNDAYI  PIC X(2).
           02  ACRDLIML    COMP  PIC  S9(4).
           02  ACRDLIMF    PICTURE X.
           02  FILLER REDEFINES ACRDLIMF.
             03 ACRDLIMA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACRDLIMI  PIC X(15).
           02  EXPYEARL    COMP  PIC  S9(4).
           02  EXPYEARF    PICTURE X.
           02  FILLER REDEFINES EXPYEARF.
             03 EXPYEARA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  EXPYEARI  PIC X(4).
           02  EXPMONL    COMP  PIC  S9(4).
           02  EXPMONF    PICTURE X.
           02  FILLER REDEFINES EXPMONF.
             03 EXPMONA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  EXPMONI  PIC X(2).
           02  EXPDAYL    COMP  PIC  S9(4).
           02  EXPDAYF    PICTURE X.
           02  FILLER REDEFINES EXPDAYF.
             03 EXPDAYA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  EXPDAYI  PIC X(2).
           02  ACSHLIML    COMP  PIC  S9(4).
           02  ACSHLIMF    PICTURE X.
           02  FILLER REDEFINES ACSHLIMF.
             03 ACSHLIMA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSHLIMI  PIC X(15).
           02  RISYEARL    COMP  PIC  S9(4).
           02  RISYEARF    PICTURE X.
           02  FILLER REDEFINES RISYEARF.
             03 RISYEARA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  RISYEARI  PIC X(4).
           02  RISMONL    COMP  PIC  S9(4).
           02  RISMONF    PICTURE X.
           02  FILLER REDEFINES RISMONF.
             03 RISMONA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  RISMONI  PIC X(2).
           02  RISDAYL    COMP  PIC  S9(4).
           02  RISDAYF    PICTURE X.
           02  FILLER REDEFINES RISDAYF.
             03 RISDAYA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  RISDAYI  PIC X(2).
           02  ACURBALL    COMP  PIC  S9(4).
           02  ACURBALF    PICTURE X.
           02  FILLER REDEFINES ACURBALF.
             03 ACURBALA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACURBALI  PIC X(15).
           02  ACRCYCRL    COMP  PIC  S9(4).
           02  ACRCYCRF    PICTURE X.
           02  FILLER REDEFINES ACRCYCRF.
             03 ACRCYCRA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACRCYCRI  PIC X(15).
           02  AADDGRPL    COMP  PIC  S9(4).
           02  AADDGRPF    PICTURE X.
           02  FILLER REDEFINES AADDGRPF.
             03 AADDGRPA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  AADDGRPI  PIC X(10).
           02  ACRCYDBL    COMP  PIC  S9(4).
           02  ACRCYDBF    PICTURE X.
           02  FILLER REDEFINES ACRCYDBF.
             03 ACRCYDBA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACRCYDBI  PIC X(15).
           02  ACSTNUML    COMP  PIC  S9(4).
           02  ACSTNUMF    PICTURE X.
           02  FILLER REDEFINES ACSTNUMF.
             03 ACSTNUMA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSTNUMI  PIC X(9).
           02  ACTSSN1L    COMP  PIC  S9(4).
           02  ACTSSN1F    PICTURE X.
           02  FILLER REDEFINES ACTSSN1F.
             03 ACTSSN1A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACTSSN1I  PIC X(3).
           02  ACTSSN2L    COMP  PIC  S9(4).
           02  ACTSSN2F    PICTURE X.
           02  FILLER REDEFINES ACTSSN2F.
             03 ACTSSN2A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACTSSN2I  PIC X(2).
           02  ACTSSN3L    COMP  PIC  S9(4).
           02  ACTSSN3F    PICTURE X.
           02  FILLER REDEFINES ACTSSN3F.
             03 ACTSSN3A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACTSSN3I  PIC X(4).
           02  DOBYEARL    COMP  PIC  S9(4).
           02  DOBYEARF    PICTURE X.
           02  FILLER REDEFINES DOBYEARF.
             03 DOBYEARA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  DOBYEARI  PIC X(4).
           02  DOBMONL    COMP  PIC  S9(4).
           02  DOBMONF    PICTURE X.
           02  FILLER REDEFINES DOBMONF.
             03 DOBMONA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  DOBMONI  PIC X(2).
           02  DOBDAYL    COMP  PIC  S9(4).
           02  DOBDAYF    PICTURE X.
           02  FILLER REDEFINES DOBDAYF.
             03 DOBDAYA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  DOBDAYI  PIC X(2).
           02  ACSTFCOL    COMP  PIC  S9(4).
           02  ACSTFCOF    PICTURE X.
           02  FILLER REDEFINES ACSTFCOF.
             03 ACSTFCOA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSTFCOI  PIC X(3).
           02  ACSFNAML    COMP  PIC  S9(4).
           02  ACSFNAMF    PICTURE X.
           02  FILLER REDEFINES ACSFNAMF.
             03 ACSFNAMA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSFNAMI  PIC X(25).
           02  ACSMNAML    COMP  PIC  S9(4).
           02  ACSMNAMF    PICTURE X.
           02  FILLER REDEFINES ACSMNAMF.
             03 ACSMNAMA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSMNAMI  PIC X(25).
           02  ACSLNAML    COMP  PIC  S9(4).
           02  ACSLNAMF    PICTURE X.
           02  FILLER REDEFINES ACSLNAMF.
             03 ACSLNAMA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSLNAMI  PIC X(25).
           02  ACSADL1L    COMP  PIC  S9(4).
           02  ACSADL1F    PICTURE X.
           02  FILLER REDEFINES ACSADL1F.
             03 ACSADL1A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSADL1I  PIC X(50).
           02  ACSSTTEL    COMP  PIC  S9(4).
           02  ACSSTTEF    PICTURE X.
           02  FILLER REDEFINES ACSSTTEF.
             03 ACSSTTEA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSSTTEI  PIC X(2).
           02  ACSADL2L    COMP  PIC  S9(4).
           02  ACSADL2F    PICTURE X.
           02  FILLER REDEFINES ACSADL2F.
             03 ACSADL2A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSADL2I  PIC X(50).
           02  ACSZIPCL    COMP  PIC  S9(4).
           02  ACSZIPCF    PICTURE X.
           02  FILLER REDEFINES ACSZIPCF.
             03 ACSZIPCA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSZIPCI  PIC X(5).
           02  ACSCITYL    COMP  PIC  S9(4).
           02  ACSCITYF    PICTURE X.
           02  FILLER REDEFINES ACSCITYF.
             03 ACSCITYA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSCITYI  PIC X(50).
           02  ACSCTRYL    COMP  PIC  S9(4).
           02  ACSCTRYF    PICTURE X.
           02  FILLER REDEFINES ACSCTRYF.
             03 ACSCTRYA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSCTRYI  PIC X(3).
           02  ACSPH1AL    COMP  PIC  S9(4).
           02  ACSPH1AF    PICTURE X.
           02  FILLER REDEFINES ACSPH1AF.
             03 ACSPH1AA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSPH1AI  PIC X(3).
           02  ACSPH1BL    COMP  PIC  S9(4).
           02  ACSPH1BF    PICTURE X.
           02  FILLER REDEFINES ACSPH1BF.
             03 ACSPH1BA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSPH1BI  PIC X(3).
           02  ACSPH1CL    COMP  PIC  S9(4).
           02  ACSPH1CF    PICTURE X.
           02  FILLER REDEFINES ACSPH1CF.
             03 ACSPH1CA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSPH1CI  PIC X(4).
           02  ACSGOVTL    COMP  PIC  S9(4).
           02  ACSGOVTF    PICTURE X.
           02  FILLER REDEFINES ACSGOVTF.
             03 ACSGOVTA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSGOVTI  PIC X(20).
           02  ACSPH2AL    COMP  PIC  S9(4).
           02  ACSPH2AF    PICTURE X.
           02  FILLER REDEFINES ACSPH2AF.
             03 ACSPH2AA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSPH2AI  PIC X(3).
           02  ACSPH2BL    COMP  PIC  S9(4).
           02  ACSPH2BF    PICTURE X.
           02  FILLER REDEFINES ACSPH2BF.
             03 ACSPH2BA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSPH2BI  PIC X(3).
           02  ACSPH2CL    COMP  PIC  S9(4).
           02  ACSPH2CF    PICTURE X.
           02  FILLER REDEFINES ACSPH2CF.
             03 ACSPH2CA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSPH2CI  PIC X(4).
           02  ACSEFTCL    COMP  PIC  S9(4).
           02  ACSEFTCF    PICTURE X.
           02  FILLER REDEFINES ACSEFTCF.
             03 ACSEFTCA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSEFTCI  PIC X(10).
           02  ACSPFLGL    COMP  PIC  S9(4).
           02  ACSPFLGF    PICTURE X.
           02  FILLER REDEFINES ACSPFLGF.
             03 ACSPFLGA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSPFLGI  PIC X(1).
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
           02  FKEY05L    COMP  PIC  S9(4).
           02  FKEY05F    PICTURE X.
           02  FILLER REDEFINES FKEY05F.
             03 FKEY05A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  FKEY05I  PIC X(7).
           02  FKEY12L    COMP  PIC  S9(4).
           02  FKEY12F    PICTURE X.
           02  FILLER REDEFINES FKEY12F.
             03 FKEY12A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  FKEY12I  PIC X(10).
       01  CACTUPAO REDEFINES CACTUPAI.
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
           02  ACSTTUSC    PICTURE X.
           02  ACSTTUSP    PICTURE X.
           02  ACSTTUSH    PICTURE X.
           02  ACSTTUSV    PICTURE X.
           02  ACSTTUSO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  OPNYEARC    PICTURE X.
           02  OPNYEARP    PICTURE X.
           02  OPNYEARH    PICTURE X.
           02  OPNYEARV    PICTURE X.
           02  OPNYEARO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  OPNMONC    PICTURE X.
           02  OPNMONP    PICTURE X.
           02  OPNMONH    PICTURE X.
           02  OPNMONV    PICTURE X.
           02  OPNMONO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  OPNDAYC    PICTURE X.
           02  OPNDAYP    PICTURE X.
           02  OPNDAYH    PICTURE X.
           02  OPNDAYV    PICTURE X.
           02  OPNDAYO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  ACRDLIMC    PICTURE X.
           02  ACRDLIMP    PICTURE X.
           02  ACRDLIMH    PICTURE X.
           02  ACRDLIMV    PICTURE X.
           02  ACRDLIMO  PIC X(15).
           02  FILLER PICTURE X(3).
           02  EXPYEARC    PICTURE X.
           02  EXPYEARP    PICTURE X.
           02  EXPYEARH    PICTURE X.
           02  EXPYEARV    PICTURE X.
           02  EXPYEARO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  EXPMONC    PICTURE X.
           02  EXPMONP    PICTURE X.
           02  EXPMONH    PICTURE X.
           02  EXPMONV    PICTURE X.
           02  EXPMONO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  EXPDAYC    PICTURE X.
           02  EXPDAYP    PICTURE X.
           02  EXPDAYH    PICTURE X.
           02  EXPDAYV    PICTURE X.
           02  EXPDAYO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  ACSHLIMC    PICTURE X.
           02  ACSHLIMP    PICTURE X.
           02  ACSHLIMH    PICTURE X.
           02  ACSHLIMV    PICTURE X.
           02  ACSHLIMO  PIC X(15).
           02  FILLER PICTURE X(3).
           02  RISYEARC    PICTURE X.
           02  RISYEARP    PICTURE X.
           02  RISYEARH    PICTURE X.
           02  RISYEARV    PICTURE X.
           02  RISYEARO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  RISMONC    PICTURE X.
           02  RISMONP    PICTURE X.
           02  RISMONH    PICTURE X.
           02  RISMONV    PICTURE X.
           02  RISMONO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  RISDAYC    PICTURE X.
           02  RISDAYP    PICTURE X.
           02  RISDAYH    PICTURE X.
           02  RISDAYV    PICTURE X.
           02  RISDAYO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  ACURBALC    PICTURE X.
           02  ACURBALP    PICTURE X.
           02  ACURBALH    PICTURE X.
           02  ACURBALV    PICTURE X.
           02  ACURBALO  PIC X(15).
           02  FILLER PICTURE X(3).
           02  ACRCYCRC    PICTURE X.
           02  ACRCYCRP    PICTURE X.
           02  ACRCYCRH    PICTURE X.
           02  ACRCYCRV    PICTURE X.
           02  ACRCYCRO  PIC X(15).
           02  FILLER PICTURE X(3).
           02  AADDGRPC    PICTURE X.
           02  AADDGRPP    PICTURE X.
           02  AADDGRPH    PICTURE X.
           02  AADDGRPV    PICTURE X.
           02  AADDGRPO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ACRCYDBC    PICTURE X.
           02  ACRCYDBP    PICTURE X.
           02  ACRCYDBH    PICTURE X.
           02  ACRCYDBV    PICTURE X.
           02  ACRCYDBO  PIC X(15).
           02  FILLER PICTURE X(3).
           02  ACSTNUMC    PICTURE X.
           02  ACSTNUMP    PICTURE X.
           02  ACSTNUMH    PICTURE X.
           02  ACSTNUMV    PICTURE X.
           02  ACSTNUMO  PIC X(9).
           02  FILLER PICTURE X(3).
           02  ACTSSN1C    PICTURE X.
           02  ACTSSN1P    PICTURE X.
           02  ACTSSN1H    PICTURE X.
           02  ACTSSN1V    PICTURE X.
           02  ACTSSN1O  PIC X(3).
           02  FILLER PICTURE X(3).
           02  ACTSSN2C    PICTURE X.
           02  ACTSSN2P    PICTURE X.
           02  ACTSSN2H    PICTURE X.
           02  ACTSSN2V    PICTURE X.
           02  ACTSSN2O  PIC X(2).
           02  FILLER PICTURE X(3).
           02  ACTSSN3C    PICTURE X.
           02  ACTSSN3P    PICTURE X.
           02  ACTSSN3H    PICTURE X.
           02  ACTSSN3V    PICTURE X.
           02  ACTSSN3O  PIC X(4).
           02  FILLER PICTURE X(3).
           02  DOBYEARC    PICTURE X.
           02  DOBYEARP    PICTURE X.
           02  DOBYEARH    PICTURE X.
           02  DOBYEARV    PICTURE X.
           02  DOBYEARO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  DOBMONC    PICTURE X.
           02  DOBMONP    PICTURE X.
           02  DOBMONH    PICTURE X.
           02  DOBMONV    PICTURE X.
           02  DOBMONO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  DOBDAYC    PICTURE X.
           02  DOBDAYP    PICTURE X.
           02  DOBDAYH    PICTURE X.
           02  DOBDAYV    PICTURE X.
           02  DOBDAYO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  ACSTFCOC    PICTURE X.
           02  ACSTFCOP    PICTURE X.
           02  ACSTFCOH    PICTURE X.
           02  ACSTFCOV    PICTURE X.
           02  ACSTFCOO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  ACSFNAMC    PICTURE X.
           02  ACSFNAMP    PICTURE X.
           02  ACSFNAMH    PICTURE X.
           02  ACSFNAMV    PICTURE X.
           02  ACSFNAMO  PIC X(25).
           02  FILLER PICTURE X(3).
           02  ACSMNAMC    PICTURE X.
           02  ACSMNAMP    PICTURE X.
           02  ACSMNAMH    PICTURE X.
           02  ACSMNAMV    PICTURE X.
           02  ACSMNAMO  PIC X(25).
           02  FILLER PICTURE X(3).
           02  ACSLNAMC    PICTURE X.
           02  ACSLNAMP    PICTURE X.
           02  ACSLNAMH    PICTURE X.
           02  ACSLNAMV    PICTURE X.
           02  ACSLNAMO  PIC X(25).
           02  FILLER PICTURE X(3).
           02  ACSADL1C    PICTURE X.
           02  ACSADL1P    PICTURE X.
           02  ACSADL1H    PICTURE X.
           02  ACSADL1V    PICTURE X.
           02  ACSADL1O  PIC X(50).
           02  FILLER PICTURE X(3).
           02  ACSSTTEC    PICTURE X.
           02  ACSSTTEP    PICTURE X.
           02  ACSSTTEH    PICTURE X.
           02  ACSSTTEV    PICTURE X.
           02  ACSSTTEO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  ACSADL2C    PICTURE X.
           02  ACSADL2P    PICTURE X.
           02  ACSADL2H    PICTURE X.
           02  ACSADL2V    PICTURE X.
           02  ACSADL2O  PIC X(50).
           02  FILLER PICTURE X(3).
           02  ACSZIPCC    PICTURE X.
           02  ACSZIPCP    PICTURE X.
           02  ACSZIPCH    PICTURE X.
           02  ACSZIPCV    PICTURE X.
           02  ACSZIPCO  PIC X(5).
           02  FILLER PICTURE X(3).
           02  ACSCITYC    PICTURE X.
           02  ACSCITYP    PICTURE X.
           02  ACSCITYH    PICTURE X.
           02  ACSCITYV    PICTURE X.
           02  ACSCITYO  PIC X(50).
           02  FILLER PICTURE X(3).
           02  ACSCTRYC    PICTURE X.
           02  ACSCTRYP    PICTURE X.
           02  ACSCTRYH    PICTURE X.
           02  ACSCTRYV    PICTURE X.
           02  ACSCTRYO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  ACSPH1AC    PICTURE X.
           02  ACSPH1AP    PICTURE X.
           02  ACSPH1AH    PICTURE X.
           02  ACSPH1AV    PICTURE X.
           02  ACSPH1AO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  ACSPH1BC    PICTURE X.
           02  ACSPH1BP    PICTURE X.
           02  ACSPH1BH    PICTURE X.
           02  ACSPH1BV    PICTURE X.
           02  ACSPH1BO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  ACSPH1CC    PICTURE X.
           02  ACSPH1CP    PICTURE X.
           02  ACSPH1CH    PICTURE X.
           02  ACSPH1CV    PICTURE X.
           02  ACSPH1CO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  ACSGOVTC    PICTURE X.
           02  ACSGOVTP    PICTURE X.
           02  ACSGOVTH    PICTURE X.
           02  ACSGOVTV    PICTURE X.
           02  ACSGOVTO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  ACSPH2AC    PICTURE X.
           02  ACSPH2AP    PICTURE X.
           02  ACSPH2AH    PICTURE X.
           02  ACSPH2AV    PICTURE X.
           02  ACSPH2AO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  ACSPH2BC    PICTURE X.
           02  ACSPH2BP    PICTURE X.
           02  ACSPH2BH    PICTURE X.
           02  ACSPH2BV    PICTURE X.
           02  ACSPH2BO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  ACSPH2CC    PICTURE X.
           02  ACSPH2CP    PICTURE X.
           02  ACSPH2CH    PICTURE X.
           02  ACSPH2CV    PICTURE X.
           02  ACSPH2CO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  ACSEFTCC    PICTURE X.
           02  ACSEFTCP    PICTURE X.
           02  ACSEFTCH    PICTURE X.
           02  ACSEFTCV    PICTURE X.
           02  ACSEFTCO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ACSPFLGC    PICTURE X.
           02  ACSPFLGP    PICTURE X.
           02  ACSPFLGH    PICTURE X.
           02  ACSPFLGV    PICTURE X.
           02  ACSPFLGO  PIC X(1).
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
           02  FKEY05C    PICTURE X.
           02  FKEY05P    PICTURE X.
           02  FKEY05H    PICTURE X.
           02  FKEY05V    PICTURE X.
           02  FKEY05O  PIC X(7).
           02  FILLER PICTURE X(3).
           02  FKEY12C    PICTURE X.
           02  FKEY12P    PICTURE X.
           02  FKEY12H    PICTURE X.
           02  FKEY12V    PICTURE X.
           02  FKEY12O  PIC X(10).
