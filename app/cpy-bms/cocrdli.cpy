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
       01  CCRDLIAI.
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
           02  CRDSEL1L    COMP  PIC  S9(4).
           02  CRDSEL1F    PICTURE X.
           02  FILLER REDEFINES CRDSEL1F.
             03 CRDSEL1A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDSEL1I  PIC X(1).
           02  ACCTNO1L    COMP  PIC  S9(4).
           02  ACCTNO1F    PICTURE X.
           02  FILLER REDEFINES ACCTNO1F.
             03 ACCTNO1A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACCTNO1I  PIC X(11).
           02  CRDNUM1L    COMP  PIC  S9(4).
           02  CRDNUM1F    PICTURE X.
           02  FILLER REDEFINES CRDNUM1F.
             03 CRDNUM1A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDNUM1I  PIC X(16).
           02  CRDSTS1L    COMP  PIC  S9(4).
           02  CRDSTS1F    PICTURE X.
           02  FILLER REDEFINES CRDSTS1F.
             03 CRDSTS1A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDSTS1I  PIC X(1).
           02  CRDSEL2L    COMP  PIC  S9(4).
           02  CRDSEL2F    PICTURE X.
           02  FILLER REDEFINES CRDSEL2F.
             03 CRDSEL2A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDSEL2I  PIC X(1).
           02  CRDSTP2L    COMP  PIC  S9(4).
           02  CRDSTP2F    PICTURE X.
           02  FILLER REDEFINES CRDSTP2F.
             03 CRDSTP2A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDSTP2I  PIC X(1).
           02  ACCTNO2L    COMP  PIC  S9(4).
           02  ACCTNO2F    PICTURE X.
           02  FILLER REDEFINES ACCTNO2F.
             03 ACCTNO2A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACCTNO2I  PIC X(11).
           02  CRDNUM2L    COMP  PIC  S9(4).
           02  CRDNUM2F    PICTURE X.
           02  FILLER REDEFINES CRDNUM2F.
             03 CRDNUM2A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDNUM2I  PIC X(16).
           02  CRDSTS2L    COMP  PIC  S9(4).
           02  CRDSTS2F    PICTURE X.
           02  FILLER REDEFINES CRDSTS2F.
             03 CRDSTS2A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDSTS2I  PIC X(1).
           02  CRDSEL3L    COMP  PIC  S9(4).
           02  CRDSEL3F    PICTURE X.
           02  FILLER REDEFINES CRDSEL3F.
             03 CRDSEL3A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDSEL3I  PIC X(1).
           02  CRDSTP3L    COMP  PIC  S9(4).
           02  CRDSTP3F    PICTURE X.
           02  FILLER REDEFINES CRDSTP3F.
             03 CRDSTP3A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDSTP3I  PIC X(1).
           02  ACCTNO3L    COMP  PIC  S9(4).
           02  ACCTNO3F    PICTURE X.
           02  FILLER REDEFINES ACCTNO3F.
             03 ACCTNO3A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACCTNO3I  PIC X(11).
           02  CRDNUM3L    COMP  PIC  S9(4).
           02  CRDNUM3F    PICTURE X.
           02  FILLER REDEFINES CRDNUM3F.
             03 CRDNUM3A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDNUM3I  PIC X(16).
           02  CRDSTS3L    COMP  PIC  S9(4).
           02  CRDSTS3F    PICTURE X.
           02  FILLER REDEFINES CRDSTS3F.
             03 CRDSTS3A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDSTS3I  PIC X(1).
           02  CRDSEL4L    COMP  PIC  S9(4).
           02  CRDSEL4F    PICTURE X.
           02  FILLER REDEFINES CRDSEL4F.
             03 CRDSEL4A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDSEL4I  PIC X(1).
           02  CRDSTP4L    COMP  PIC  S9(4).
           02  CRDSTP4F    PICTURE X.
           02  FILLER REDEFINES CRDSTP4F.
             03 CRDSTP4A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDSTP4I  PIC X(1).
           02  ACCTNO4L    COMP  PIC  S9(4).
           02  ACCTNO4F    PICTURE X.
           02  FILLER REDEFINES ACCTNO4F.
             03 ACCTNO4A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACCTNO4I  PIC X(11).
           02  CRDNUM4L    COMP  PIC  S9(4).
           02  CRDNUM4F    PICTURE X.
           02  FILLER REDEFINES CRDNUM4F.
             03 CRDNUM4A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDNUM4I  PIC X(16).
           02  CRDSTS4L    COMP  PIC  S9(4).
           02  CRDSTS4F    PICTURE X.
           02  FILLER REDEFINES CRDSTS4F.
             03 CRDSTS4A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDSTS4I  PIC X(1).
           02  CRDSEL5L    COMP  PIC  S9(4).
           02  CRDSEL5F    PICTURE X.
           02  FILLER REDEFINES CRDSEL5F.
             03 CRDSEL5A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDSEL5I  PIC X(1).
           02  CRDSTP5L    COMP  PIC  S9(4).
           02  CRDSTP5F    PICTURE X.
           02  FILLER REDEFINES CRDSTP5F.
             03 CRDSTP5A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDSTP5I  PIC X(1).
           02  ACCTNO5L    COMP  PIC  S9(4).
           02  ACCTNO5F    PICTURE X.
           02  FILLER REDEFINES ACCTNO5F.
             03 ACCTNO5A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACCTNO5I  PIC X(11).
           02  CRDNUM5L    COMP  PIC  S9(4).
           02  CRDNUM5F    PICTURE X.
           02  FILLER REDEFINES CRDNUM5F.
             03 CRDNUM5A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDNUM5I  PIC X(16).
           02  CRDSTS5L    COMP  PIC  S9(4).
           02  CRDSTS5F    PICTURE X.
           02  FILLER REDEFINES CRDSTS5F.
             03 CRDSTS5A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDSTS5I  PIC X(1).
           02  CRDSEL6L    COMP  PIC  S9(4).
           02  CRDSEL6F    PICTURE X.
           02  FILLER REDEFINES CRDSEL6F.
             03 CRDSEL6A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDSEL6I  PIC X(1).
           02  CRDSTP6L    COMP  PIC  S9(4).
           02  CRDSTP6F    PICTURE X.
           02  FILLER REDEFINES CRDSTP6F.
             03 CRDSTP6A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDSTP6I  PIC X(1).
           02  ACCTNO6L    COMP  PIC  S9(4).
           02  ACCTNO6F    PICTURE X.
           02  FILLER REDEFINES ACCTNO6F.
             03 ACCTNO6A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACCTNO6I  PIC X(11).
           02  CRDNUM6L    COMP  PIC  S9(4).
           02  CRDNUM6F    PICTURE X.
           02  FILLER REDEFINES CRDNUM6F.
             03 CRDNUM6A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDNUM6I  PIC X(16).
           02  CRDSTS6L    COMP  PIC  S9(4).
           02  CRDSTS6F    PICTURE X.
           02  FILLER REDEFINES CRDSTS6F.
             03 CRDSTS6A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDSTS6I  PIC X(1).
           02  CRDSEL7L    COMP  PIC  S9(4).
           02  CRDSEL7F    PICTURE X.
           02  FILLER REDEFINES CRDSEL7F.
             03 CRDSEL7A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDSEL7I  PIC X(1).
           02  CRDSTP7L    COMP  PIC  S9(4).
           02  CRDSTP7F    PICTURE X.
           02  FILLER REDEFINES CRDSTP7F.
             03 CRDSTP7A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDSTP7I  PIC X(1).
           02  ACCTNO7L    COMP  PIC  S9(4).
           02  ACCTNO7F    PICTURE X.
           02  FILLER REDEFINES ACCTNO7F.
             03 ACCTNO7A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ACCTNO7I  PIC X(11).
           02  CRDNUM7L    COMP  PIC  S9(4).
           02  CRDNUM7F    PICTURE X.
           02  FILLER REDEFINES CRDNUM7F.
             03 CRDNUM7A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDNUM7I  PIC X(16).
           02  CRDSTS7L    COMP  PIC  S9(4).
           02  CRDSTS7F    PICTURE X.
           02  FILLER REDEFINES CRDSTS7F.
             03 CRDSTS7A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CRDSTS7I  PIC X(1).
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
       01  CCRDLIAO REDEFINES CCRDLIAI.
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
           02  CRDSEL1C    PICTURE X.
           02  CRDSEL1P    PICTURE X.
           02  CRDSEL1H    PICTURE X.
           02  CRDSEL1V    PICTURE X.
           02  CRDSEL1O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ACCTNO1C    PICTURE X.
           02  ACCTNO1P    PICTURE X.
           02  ACCTNO1H    PICTURE X.
           02  ACCTNO1V    PICTURE X.
           02  ACCTNO1O  PIC X(11).
           02  FILLER PICTURE X(3).
           02  CRDNUM1C    PICTURE X.
           02  CRDNUM1P    PICTURE X.
           02  CRDNUM1H    PICTURE X.
           02  CRDNUM1V    PICTURE X.
           02  CRDNUM1O  PIC X(16).
           02  FILLER PICTURE X(3).
           02  CRDSTS1C    PICTURE X.
           02  CRDSTS1P    PICTURE X.
           02  CRDSTS1H    PICTURE X.
           02  CRDSTS1V    PICTURE X.
           02  CRDSTS1O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  CRDSEL2C    PICTURE X.
           02  CRDSEL2P    PICTURE X.
           02  CRDSEL2H    PICTURE X.
           02  CRDSEL2V    PICTURE X.
           02  CRDSEL2O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  CRDSTP2C    PICTURE X.
           02  CRDSTP2P    PICTURE X.
           02  CRDSTP2H    PICTURE X.
           02  CRDSTP2V    PICTURE X.
           02  CRDSTP2O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ACCTNO2C    PICTURE X.
           02  ACCTNO2P    PICTURE X.
           02  ACCTNO2H    PICTURE X.
           02  ACCTNO2V    PICTURE X.
           02  ACCTNO2O  PIC X(11).
           02  FILLER PICTURE X(3).
           02  CRDNUM2C    PICTURE X.
           02  CRDNUM2P    PICTURE X.
           02  CRDNUM2H    PICTURE X.
           02  CRDNUM2V    PICTURE X.
           02  CRDNUM2O  PIC X(16).
           02  FILLER PICTURE X(3).
           02  CRDSTS2C    PICTURE X.
           02  CRDSTS2P    PICTURE X.
           02  CRDSTS2H    PICTURE X.
           02  CRDSTS2V    PICTURE X.
           02  CRDSTS2O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  CRDSEL3C    PICTURE X.
           02  CRDSEL3P    PICTURE X.
           02  CRDSEL3H    PICTURE X.
           02  CRDSEL3V    PICTURE X.
           02  CRDSEL3O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  CRDSTP3C    PICTURE X.
           02  CRDSTP3P    PICTURE X.
           02  CRDSTP3H    PICTURE X.
           02  CRDSTP3V    PICTURE X.
           02  CRDSTP3O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ACCTNO3C    PICTURE X.
           02  ACCTNO3P    PICTURE X.
           02  ACCTNO3H    PICTURE X.
           02  ACCTNO3V    PICTURE X.
           02  ACCTNO3O  PIC X(11).
           02  FILLER PICTURE X(3).
           02  CRDNUM3C    PICTURE X.
           02  CRDNUM3P    PICTURE X.
           02  CRDNUM3H    PICTURE X.
           02  CRDNUM3V    PICTURE X.
           02  CRDNUM3O  PIC X(16).
           02  FILLER PICTURE X(3).
           02  CRDSTS3C    PICTURE X.
           02  CRDSTS3P    PICTURE X.
           02  CRDSTS3H    PICTURE X.
           02  CRDSTS3V    PICTURE X.
           02  CRDSTS3O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  CRDSEL4C    PICTURE X.
           02  CRDSEL4P    PICTURE X.
           02  CRDSEL4H    PICTURE X.
           02  CRDSEL4V    PICTURE X.
           02  CRDSEL4O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  CRDSTP4C    PICTURE X.
           02  CRDSTP4P    PICTURE X.
           02  CRDSTP4H    PICTURE X.
           02  CRDSTP4V    PICTURE X.
           02  CRDSTP4O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ACCTNO4C    PICTURE X.
           02  ACCTNO4P    PICTURE X.
           02  ACCTNO4H    PICTURE X.
           02  ACCTNO4V    PICTURE X.
           02  ACCTNO4O  PIC X(11).
           02  FILLER PICTURE X(3).
           02  CRDNUM4C    PICTURE X.
           02  CRDNUM4P    PICTURE X.
           02  CRDNUM4H    PICTURE X.
           02  CRDNUM4V    PICTURE X.
           02  CRDNUM4O  PIC X(16).
           02  FILLER PICTURE X(3).
           02  CRDSTS4C    PICTURE X.
           02  CRDSTS4P    PICTURE X.
           02  CRDSTS4H    PICTURE X.
           02  CRDSTS4V    PICTURE X.
           02  CRDSTS4O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  CRDSEL5C    PICTURE X.
           02  CRDSEL5P    PICTURE X.
           02  CRDSEL5H    PICTURE X.
           02  CRDSEL5V    PICTURE X.
           02  CRDSEL5O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  CRDSTP5C    PICTURE X.
           02  CRDSTP5P    PICTURE X.
           02  CRDSTP5H    PICTURE X.
           02  CRDSTP5V    PICTURE X.
           02  CRDSTP5O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ACCTNO5C    PICTURE X.
           02  ACCTNO5P    PICTURE X.
           02  ACCTNO5H    PICTURE X.
           02  ACCTNO5V    PICTURE X.
           02  ACCTNO5O  PIC X(11).
           02  FILLER PICTURE X(3).
           02  CRDNUM5C    PICTURE X.
           02  CRDNUM5P    PICTURE X.
           02  CRDNUM5H    PICTURE X.
           02  CRDNUM5V    PICTURE X.
           02  CRDNUM5O  PIC X(16).
           02  FILLER PICTURE X(3).
           02  CRDSTS5C    PICTURE X.
           02  CRDSTS5P    PICTURE X.
           02  CRDSTS5H    PICTURE X.
           02  CRDSTS5V    PICTURE X.
           02  CRDSTS5O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  CRDSEL6C    PICTURE X.
           02  CRDSEL6P    PICTURE X.
           02  CRDSEL6H    PICTURE X.
           02  CRDSEL6V    PICTURE X.
           02  CRDSEL6O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  CRDSTP6C    PICTURE X.
           02  CRDSTP6P    PICTURE X.
           02  CRDSTP6H    PICTURE X.
           02  CRDSTP6V    PICTURE X.
           02  CRDSTP6O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ACCTNO6C    PICTURE X.
           02  ACCTNO6P    PICTURE X.
           02  ACCTNO6H    PICTURE X.
           02  ACCTNO6V    PICTURE X.
           02  ACCTNO6O  PIC X(11).
           02  FILLER PICTURE X(3).
           02  CRDNUM6C    PICTURE X.
           02  CRDNUM6P    PICTURE X.
           02  CRDNUM6H    PICTURE X.
           02  CRDNUM6V    PICTURE X.
           02  CRDNUM6O  PIC X(16).
           02  FILLER PICTURE X(3).
           02  CRDSTS6C    PICTURE X.
           02  CRDSTS6P    PICTURE X.
           02  CRDSTS6H    PICTURE X.
           02  CRDSTS6V    PICTURE X.
           02  CRDSTS6O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  CRDSEL7C    PICTURE X.
           02  CRDSEL7P    PICTURE X.
           02  CRDSEL7H    PICTURE X.
           02  CRDSEL7V    PICTURE X.
           02  CRDSEL7O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  CRDSTP7C    PICTURE X.
           02  CRDSTP7P    PICTURE X.
           02  CRDSTP7H    PICTURE X.
           02  CRDSTP7V    PICTURE X.
           02  CRDSTP7O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ACCTNO7C    PICTURE X.
           02  ACCTNO7P    PICTURE X.
           02  ACCTNO7H    PICTURE X.
           02  ACCTNO7V    PICTURE X.
           02  ACCTNO7O  PIC X(11).
           02  FILLER PICTURE X(3).
           02  CRDNUM7C    PICTURE X.
           02  CRDNUM7P    PICTURE X.
           02  CRDNUM7H    PICTURE X.
           02  CRDNUM7V    PICTURE X.
           02  CRDNUM7O  PIC X(16).
           02  FILLER PICTURE X(3).
           02  CRDSTS7C    PICTURE X.
           02  CRDSTS7P    PICTURE X.
           02  CRDSTS7H    PICTURE X.
           02  CRDSTS7V    PICTURE X.
           02  CRDSTS7O  PIC X(1).
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
