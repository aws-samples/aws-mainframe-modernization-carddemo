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
       01  CACTVWAI.
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
           02  ACCTSIDI  PIC 99999999999.
           02  ACSTTUSL    COMP  PIC  S9(4).
           02  ACSTTUSF    PICTURE X.
           02  FILLER REDEFINES ACSTTUSF.
             03 ACSTTUSA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSTTUSI  PIC X(1).
           02  ADTOPENL    COMP  PIC  S9(4).
           02  ADTOPENF    PICTURE X.
           02  FILLER REDEFINES ADTOPENF.
             03 ADTOPENA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ADTOPENI  PIC X(10).
           02  ACRDLIML    COMP  PIC  S9(4).
           02  ACRDLIMF    PICTURE X.
           02  FILLER REDEFINES ACRDLIMF.
             03 ACRDLIMA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACRDLIMI  PIC X(15).
           02  AEXPDTL    COMP  PIC  S9(4).
           02  AEXPDTF    PICTURE X.
           02  FILLER REDEFINES AEXPDTF.
             03 AEXPDTA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  AEXPDTI  PIC X(10).
           02  ACSHLIML    COMP  PIC  S9(4).
           02  ACSHLIMF    PICTURE X.
           02  FILLER REDEFINES ACSHLIMF.
             03 ACSHLIMA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSHLIMI  PIC X(15).
           02  AREISDTL    COMP  PIC  S9(4).
           02  AREISDTF    PICTURE X.
           02  FILLER REDEFINES AREISDTF.
             03 AREISDTA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  AREISDTI  PIC X(10).
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
           02  ACSTSSNL    COMP  PIC  S9(4).
           02  ACSTSSNF    PICTURE X.
           02  FILLER REDEFINES ACSTSSNF.
             03 ACSTSSNA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSTSSNI  PIC X(12).
           02  ACSTDOBL    COMP  PIC  S9(4).
           02  ACSTDOBF    PICTURE X.
           02  FILLER REDEFINES ACSTDOBF.
             03 ACSTDOBA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSTDOBI  PIC X(10).
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
           02  ACSPHN1L    COMP  PIC  S9(4).
           02  ACSPHN1F    PICTURE X.
           02  FILLER REDEFINES ACSPHN1F.
             03 ACSPHN1A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSPHN1I  PIC X(13).
           02  ACSGOVTL    COMP  PIC  S9(4).
           02  ACSGOVTF    PICTURE X.
           02  FILLER REDEFINES ACSGOVTF.
             03 ACSGOVTA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSGOVTI  PIC X(20).
           02  ACSPHN2L    COMP  PIC  S9(4).
           02  ACSPHN2F    PICTURE X.
           02  FILLER REDEFINES ACSPHN2F.
             03 ACSPHN2A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACSPHN2I  PIC X(13).
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
       01  CACTVWAO REDEFINES CACTVWAI.
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
           02  ADTOPENC    PICTURE X.
           02  ADTOPENP    PICTURE X.
           02  ADTOPENH    PICTURE X.
           02  ADTOPENV    PICTURE X.
           02  ADTOPENO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ACRDLIMC    PICTURE X.
           02  ACRDLIMP    PICTURE X.
           02  ACRDLIMH    PICTURE X.
           02  ACRDLIMV    PICTURE X.
           02  ACRDLIMO PIC +ZZZ,ZZZ,ZZZ.99.
           02  FILLER PICTURE X(3).
           02  AEXPDTC    PICTURE X.
           02  AEXPDTP    PICTURE X.
           02  AEXPDTH    PICTURE X.
           02  AEXPDTV    PICTURE X.
           02  AEXPDTO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ACSHLIMC    PICTURE X.
           02  ACSHLIMP    PICTURE X.
           02  ACSHLIMH    PICTURE X.
           02  ACSHLIMV    PICTURE X.
           02  ACSHLIMO PIC +ZZZ,ZZZ,ZZZ.99.
           02  FILLER PICTURE X(3).
           02  AREISDTC    PICTURE X.
           02  AREISDTP    PICTURE X.
           02  AREISDTH    PICTURE X.
           02  AREISDTV    PICTURE X.
           02  AREISDTO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ACURBALC    PICTURE X.
           02  ACURBALP    PICTURE X.
           02  ACURBALH    PICTURE X.
           02  ACURBALV    PICTURE X.
           02  ACURBALO PIC +ZZZ,ZZZ,ZZZ.99.
           02  FILLER PICTURE X(3).
           02  ACRCYCRC    PICTURE X.
           02  ACRCYCRP    PICTURE X.
           02  ACRCYCRH    PICTURE X.
           02  ACRCYCRV    PICTURE X.
           02  ACRCYCRO PIC +ZZZ,ZZZ,ZZZ.99.
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
           02  ACRCYDBO PIC +ZZZ,ZZZ,ZZZ.99.
           02  FILLER PICTURE X(3).
           02  ACSTNUMC    PICTURE X.
           02  ACSTNUMP    PICTURE X.
           02  ACSTNUMH    PICTURE X.
           02  ACSTNUMV    PICTURE X.
           02  ACSTNUMO  PIC X(9).
           02  FILLER PICTURE X(3).
           02  ACSTSSNC    PICTURE X.
           02  ACSTSSNP    PICTURE X.
           02  ACSTSSNH    PICTURE X.
           02  ACSTSSNV    PICTURE X.
           02  ACSTSSNO  PIC X(12).
           02  FILLER PICTURE X(3).
           02  ACSTDOBC    PICTURE X.
           02  ACSTDOBP    PICTURE X.
           02  ACSTDOBH    PICTURE X.
           02  ACSTDOBV    PICTURE X.
           02  ACSTDOBO  PIC X(10).
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
           02  ACSPHN1C    PICTURE X.
           02  ACSPHN1P    PICTURE X.
           02  ACSPHN1H    PICTURE X.
           02  ACSPHN1V    PICTURE X.
           02  ACSPHN1O  PIC X(13).
           02  FILLER PICTURE X(3).
           02  ACSGOVTC    PICTURE X.
           02  ACSGOVTP    PICTURE X.
           02  ACSGOVTH    PICTURE X.
           02  ACSGOVTV    PICTURE X.
           02  ACSGOVTO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  ACSPHN2C    PICTURE X.
           02  ACSPHN2P    PICTURE X.
           02  ACSPHN2H    PICTURE X.
           02  ACSPHN2V    PICTURE X.
           02  ACSPHN2O  PIC X(13).
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
