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
       01  CTRTLIAI.
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
           02  PAGENOL    COMP  PIC  S9(4).
           02  PAGENOF    PICTURE X.
           02  FILLER REDEFINES PAGENOF.
             03 PAGENOA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  PAGENOI  PIC X(3).
           02  TRTYPEL    COMP  PIC  S9(4).
           02  TRTYPEF    PICTURE X.
           02  FILLER REDEFINES TRTYPEF.
             03 TRTYPEA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTYPEI  PIC X(2).
           02  TRDESCL    COMP  PIC  S9(4).
           02  TRDESCF    PICTURE X.
           02  FILLER REDEFINES TRDESCF.
             03 TRDESCA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRDESCI  PIC X(50).
           02  TRTSEL1L    COMP  PIC  S9(4).
           02  TRTSEL1F    PICTURE X.
           02  FILLER REDEFINES TRTSEL1F.
             03 TRTSEL1A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTSEL1I  PIC X(1).
           02  TRTTYP1L    COMP  PIC  S9(4).
           02  TRTTYP1F    PICTURE X.
           02  FILLER REDEFINES TRTTYP1F.
             03 TRTTYP1A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTTYP1I  PIC X(2).
           02  TRTYPD1L    COMP  PIC  S9(4).
           02  TRTYPD1F    PICTURE X.
           02  FILLER REDEFINES TRTYPD1F.
             03 TRTYPD1A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTYPD1I  PIC X(50).
           02  TRTSEL2L    COMP  PIC  S9(4).
           02  TRTSEL2F    PICTURE X.
           02  FILLER REDEFINES TRTSEL2F.
             03 TRTSEL2A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTSEL2I  PIC X(1).
           02  TRTTYP2L    COMP  PIC  S9(4).
           02  TRTTYP2F    PICTURE X.
           02  FILLER REDEFINES TRTTYP2F.
             03 TRTTYP2A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTTYP2I  PIC X(2).
           02  TRTYPD2L    COMP  PIC  S9(4).
           02  TRTYPD2F    PICTURE X.
           02  FILLER REDEFINES TRTYPD2F.
             03 TRTYPD2A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTYPD2I  PIC X(50).
           02  TRTSEL3L    COMP  PIC  S9(4).
           02  TRTSEL3F    PICTURE X.
           02  FILLER REDEFINES TRTSEL3F.
             03 TRTSEL3A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTSEL3I  PIC X(1).
           02  TRTTYP3L    COMP  PIC  S9(4).
           02  TRTTYP3F    PICTURE X.
           02  FILLER REDEFINES TRTTYP3F.
             03 TRTTYP3A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTTYP3I  PIC X(2).
           02  TRTYPD3L    COMP  PIC  S9(4).
           02  TRTYPD3F    PICTURE X.
           02  FILLER REDEFINES TRTYPD3F.
             03 TRTYPD3A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTYPD3I  PIC X(50).
           02  TRTSEL4L    COMP  PIC  S9(4).
           02  TRTSEL4F    PICTURE X.
           02  FILLER REDEFINES TRTSEL4F.
             03 TRTSEL4A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTSEL4I  PIC X(1).
           02  TRTTYP4L    COMP  PIC  S9(4).
           02  TRTTYP4F    PICTURE X.
           02  FILLER REDEFINES TRTTYP4F.
             03 TRTTYP4A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTTYP4I  PIC X(2).
           02  TRTYPD4L    COMP  PIC  S9(4).
           02  TRTYPD4F    PICTURE X.
           02  FILLER REDEFINES TRTYPD4F.
             03 TRTYPD4A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTYPD4I  PIC X(50).
           02  TRTSEL5L    COMP  PIC  S9(4).
           02  TRTSEL5F    PICTURE X.
           02  FILLER REDEFINES TRTSEL5F.
             03 TRTSEL5A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTSEL5I  PIC X(1).
           02  TRTTYP5L    COMP  PIC  S9(4).
           02  TRTTYP5F    PICTURE X.
           02  FILLER REDEFINES TRTTYP5F.
             03 TRTTYP5A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTTYP5I  PIC X(2).
           02  TRTYPD5L    COMP  PIC  S9(4).
           02  TRTYPD5F    PICTURE X.
           02  FILLER REDEFINES TRTYPD5F.
             03 TRTYPD5A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTYPD5I  PIC X(50).
           02  TRTSEL6L    COMP  PIC  S9(4).
           02  TRTSEL6F    PICTURE X.
           02  FILLER REDEFINES TRTSEL6F.
             03 TRTSEL6A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTSEL6I  PIC X(1).
           02  TRTTYP6L    COMP  PIC  S9(4).
           02  TRTTYP6F    PICTURE X.
           02  FILLER REDEFINES TRTTYP6F.
             03 TRTTYP6A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTTYP6I  PIC X(2).
           02  TRTYPD6L    COMP  PIC  S9(4).
           02  TRTYPD6F    PICTURE X.
           02  FILLER REDEFINES TRTYPD6F.
             03 TRTYPD6A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTYPD6I  PIC X(50).
           02  TRTSEL7L    COMP  PIC  S9(4).
           02  TRTSEL7F    PICTURE X.
           02  FILLER REDEFINES TRTSEL7F.
             03 TRTSEL7A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTSEL7I  PIC X(1).
           02  TRTTYP7L    COMP  PIC  S9(4).
           02  TRTTYP7F    PICTURE X.
           02  FILLER REDEFINES TRTTYP7F.
             03 TRTTYP7A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTTYP7I  PIC X(2).
           02  TRTYPD7L    COMP  PIC  S9(4).
           02  TRTYPD7F    PICTURE X.
           02  FILLER REDEFINES TRTYPD7F.
             03 TRTYPD7A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTYPD7I  PIC X(50).
           02  TRTSELAL    COMP  PIC  S9(4).
           02  TRTSELAF    PICTURE X.
           02  FILLER REDEFINES TRTSELAF.
             03 TRTSELAA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTSELAI  PIC X(1).
           02  TRTTYPAL    COMP  PIC  S9(4).
           02  TRTTYPAF    PICTURE X.
           02  FILLER REDEFINES TRTTYPAF.
             03 TRTTYPAA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTTYPAI  PIC X(2).
           02  TRTDSCAL    COMP  PIC  S9(4).
           02  TRTDSCAF    PICTURE X.
           02  FILLER REDEFINES TRTDSCAF.
             03 TRTDSCAA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRTDSCAI  PIC X(50).
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
           02  BUTNF02L    COMP  PIC  S9(4).
           02  BUTNF02F    PICTURE X.
           02  FILLER REDEFINES BUTNF02F.
             03 BUTNF02A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  BUTNF02I  PIC X(7).
           02  BUTNF03L    COMP  PIC  S9(4).
           02  BUTNF03F    PICTURE X.
           02  FILLER REDEFINES BUTNF03F.
             03 BUTNF03A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  BUTNF03I  PIC X(7).
           02  BUTNF07L    COMP  PIC  S9(4).
           02  BUTNF07F    PICTURE X.
           02  FILLER REDEFINES BUTNF07F.
             03 BUTNF07A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  BUTNF07I  PIC X(10).
           02  BUTNF08L    COMP  PIC  S9(4).
           02  BUTNF08F    PICTURE X.
           02  FILLER REDEFINES BUTNF08F.
             03 BUTNF08A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  BUTNF08I  PIC X(10).
           02  BUTNF10L    COMP  PIC  S9(4).
           02  BUTNF10F    PICTURE X.
           02  FILLER REDEFINES BUTNF10F.
             03 BUTNF10A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  BUTNF10I  PIC X(8).
       01  CTRTLIAO REDEFINES CTRTLIAI.
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
           02  PAGENOC    PICTURE X.
           02  PAGENOP    PICTURE X.
           02  PAGENOH    PICTURE X.
           02  PAGENOV    PICTURE X.
           02  PAGENOO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  TRTYPEC    PICTURE X.
           02  TRTYPEP    PICTURE X.
           02  TRTYPEH    PICTURE X.
           02  TRTYPEV    PICTURE X.
           02  TRTYPEO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  TRDESCC    PICTURE X.
           02  TRDESCP    PICTURE X.
           02  TRDESCH    PICTURE X.
           02  TRDESCV    PICTURE X.
           02  TRDESCO  PIC X(50).
           02  FILLER PICTURE X(3).
           02  TRTSEL1C    PICTURE X.
           02  TRTSEL1P    PICTURE X.
           02  TRTSEL1H    PICTURE X.
           02  TRTSEL1V    PICTURE X.
           02  TRTSEL1O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  TRTTYP1C    PICTURE X.
           02  TRTTYP1P    PICTURE X.
           02  TRTTYP1H    PICTURE X.
           02  TRTTYP1V    PICTURE X.
           02  TRTTYP1O  PIC X(2).
           02  FILLER PICTURE X(3).
           02  TRTYPD1C    PICTURE X.
           02  TRTYPD1P    PICTURE X.
           02  TRTYPD1H    PICTURE X.
           02  TRTYPD1V    PICTURE X.
           02  TRTYPD1O  PIC X(50).
           02  FILLER PICTURE X(3).
           02  TRTSEL2C    PICTURE X.
           02  TRTSEL2P    PICTURE X.
           02  TRTSEL2H    PICTURE X.
           02  TRTSEL2V    PICTURE X.
           02  TRTSEL2O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  TRTTYP2C    PICTURE X.
           02  TRTTYP2P    PICTURE X.
           02  TRTTYP2H    PICTURE X.
           02  TRTTYP2V    PICTURE X.
           02  TRTTYP2O  PIC X(2).
           02  FILLER PICTURE X(3).
           02  TRTYPD2C    PICTURE X.
           02  TRTYPD2P    PICTURE X.
           02  TRTYPD2H    PICTURE X.
           02  TRTYPD2V    PICTURE X.
           02  TRTYPD2O  PIC X(50).
           02  FILLER PICTURE X(3).
           02  TRTSEL3C    PICTURE X.
           02  TRTSEL3P    PICTURE X.
           02  TRTSEL3H    PICTURE X.
           02  TRTSEL3V    PICTURE X.
           02  TRTSEL3O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  TRTTYP3C    PICTURE X.
           02  TRTTYP3P    PICTURE X.
           02  TRTTYP3H    PICTURE X.
           02  TRTTYP3V    PICTURE X.
           02  TRTTYP3O  PIC X(2).
           02  FILLER PICTURE X(3).
           02  TRTYPD3C    PICTURE X.
           02  TRTYPD3P    PICTURE X.
           02  TRTYPD3H    PICTURE X.
           02  TRTYPD3V    PICTURE X.
           02  TRTYPD3O  PIC X(50).
           02  FILLER PICTURE X(3).
           02  TRTSEL4C    PICTURE X.
           02  TRTSEL4P    PICTURE X.
           02  TRTSEL4H    PICTURE X.
           02  TRTSEL4V    PICTURE X.
           02  TRTSEL4O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  TRTTYP4C    PICTURE X.
           02  TRTTYP4P    PICTURE X.
           02  TRTTYP4H    PICTURE X.
           02  TRTTYP4V    PICTURE X.
           02  TRTTYP4O  PIC X(2).
           02  FILLER PICTURE X(3).
           02  TRTYPD4C    PICTURE X.
           02  TRTYPD4P    PICTURE X.
           02  TRTYPD4H    PICTURE X.
           02  TRTYPD4V    PICTURE X.
           02  TRTYPD4O  PIC X(50).
           02  FILLER PICTURE X(3).
           02  TRTSEL5C    PICTURE X.
           02  TRTSEL5P    PICTURE X.
           02  TRTSEL5H    PICTURE X.
           02  TRTSEL5V    PICTURE X.
           02  TRTSEL5O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  TRTTYP5C    PICTURE X.
           02  TRTTYP5P    PICTURE X.
           02  TRTTYP5H    PICTURE X.
           02  TRTTYP5V    PICTURE X.
           02  TRTTYP5O  PIC X(2).
           02  FILLER PICTURE X(3).
           02  TRTYPD5C    PICTURE X.
           02  TRTYPD5P    PICTURE X.
           02  TRTYPD5H    PICTURE X.
           02  TRTYPD5V    PICTURE X.
           02  TRTYPD5O  PIC X(50).
           02  FILLER PICTURE X(3).
           02  TRTSEL6C    PICTURE X.
           02  TRTSEL6P    PICTURE X.
           02  TRTSEL6H    PICTURE X.
           02  TRTSEL6V    PICTURE X.
           02  TRTSEL6O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  TRTTYP6C    PICTURE X.
           02  TRTTYP6P    PICTURE X.
           02  TRTTYP6H    PICTURE X.
           02  TRTTYP6V    PICTURE X.
           02  TRTTYP6O  PIC X(2).
           02  FILLER PICTURE X(3).
           02  TRTYPD6C    PICTURE X.
           02  TRTYPD6P    PICTURE X.
           02  TRTYPD6H    PICTURE X.
           02  TRTYPD6V    PICTURE X.
           02  TRTYPD6O  PIC X(50).
           02  FILLER PICTURE X(3).
           02  TRTSEL7C    PICTURE X.
           02  TRTSEL7P    PICTURE X.
           02  TRTSEL7H    PICTURE X.
           02  TRTSEL7V    PICTURE X.
           02  TRTSEL7O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  TRTTYP7C    PICTURE X.
           02  TRTTYP7P    PICTURE X.
           02  TRTTYP7H    PICTURE X.
           02  TRTTYP7V    PICTURE X.
           02  TRTTYP7O  PIC X(2).
           02  FILLER PICTURE X(3).
           02  TRTYPD7C    PICTURE X.
           02  TRTYPD7P    PICTURE X.
           02  TRTYPD7H    PICTURE X.
           02  TRTYPD7V    PICTURE X.
           02  TRTYPD7O  PIC X(50).
           02  FILLER PICTURE X(3).
           02  TRTSELAC    PICTURE X.
           02  TRTSELAP    PICTURE X.
           02  TRTSELAH    PICTURE X.
           02  TRTSELAV    PICTURE X.
           02  TRTSELAO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  TRTTYPAC    PICTURE X.
           02  TRTTYPAP    PICTURE X.
           02  TRTTYPAH    PICTURE X.
           02  TRTTYPAV    PICTURE X.
           02  TRTTYPAO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  TRTDSCAC    PICTURE X.
           02  TRTDSCAP    PICTURE X.
           02  TRTDSCAH    PICTURE X.
           02  TRTDSCAV    PICTURE X.
           02  TRTDSCAO  PIC X(50).
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
           02  BUTNF02C    PICTURE X.
           02  BUTNF02P    PICTURE X.
           02  BUTNF02H    PICTURE X.
           02  BUTNF02V    PICTURE X.
           02  BUTNF02O  PIC X(7).
           02  FILLER PICTURE X(3).
           02  BUTNF03C    PICTURE X.
           02  BUTNF03P    PICTURE X.
           02  BUTNF03H    PICTURE X.
           02  BUTNF03V    PICTURE X.
           02  BUTNF03O  PIC X(7).
           02  FILLER PICTURE X(3).
           02  BUTNF07C    PICTURE X.
           02  BUTNF07P    PICTURE X.
           02  BUTNF07H    PICTURE X.
           02  BUTNF07V    PICTURE X.
           02  BUTNF07O  PIC X(10).
           02  FILLER PICTURE X(3).
           02  BUTNF08C    PICTURE X.
           02  BUTNF08P    PICTURE X.
           02  BUTNF08H    PICTURE X.
           02  BUTNF08V    PICTURE X.
           02  BUTNF08O  PIC X(10).
           02  FILLER PICTURE X(3).
           02  BUTNF10C    PICTURE X.
           02  BUTNF10P    PICTURE X.
           02  BUTNF10H    PICTURE X.
           02  BUTNF10V    PICTURE X.
           02  BUTNF10O  PIC X(8).
