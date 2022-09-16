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
       01  COTRN0AI.
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
           02  PAGENUML    COMP  PIC  S9(4).
           02  PAGENUMF    PICTURE X.
           02  FILLER REDEFINES PAGENUMF.
             03 PAGENUMA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  PAGENUMI  PIC X(8).
           02  TRNIDINL    COMP  PIC  S9(4).
           02  TRNIDINF    PICTURE X.
           02  FILLER REDEFINES TRNIDINF.
             03 TRNIDINA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRNIDINI  PIC X(16).
           02  SEL0001L    COMP  PIC  S9(4).
           02  SEL0001F    PICTURE X.
           02  FILLER REDEFINES SEL0001F.
             03 SEL0001A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  SEL0001I  PIC X(1).
           02  TRNID01L    COMP  PIC  S9(4).
           02  TRNID01F    PICTURE X.
           02  FILLER REDEFINES TRNID01F.
             03 TRNID01A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRNID01I  PIC X(16).
           02  TDATE01L    COMP  PIC  S9(4).
           02  TDATE01F    PICTURE X.
           02  FILLER REDEFINES TDATE01F.
             03 TDATE01A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TDATE01I  PIC X(8).
           02  TDESC01L    COMP  PIC  S9(4).
           02  TDESC01F    PICTURE X.
           02  FILLER REDEFINES TDESC01F.
             03 TDESC01A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TDESC01I  PIC X(26).
           02  TAMT001L    COMP  PIC  S9(4).
           02  TAMT001F    PICTURE X.
           02  FILLER REDEFINES TAMT001F.
             03 TAMT001A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TAMT001I  PIC X(12).
           02  SEL0002L    COMP  PIC  S9(4).
           02  SEL0002F    PICTURE X.
           02  FILLER REDEFINES SEL0002F.
             03 SEL0002A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  SEL0002I  PIC X(1).
           02  TRNID02L    COMP  PIC  S9(4).
           02  TRNID02F    PICTURE X.
           02  FILLER REDEFINES TRNID02F.
             03 TRNID02A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRNID02I  PIC X(16).
           02  TDATE02L    COMP  PIC  S9(4).
           02  TDATE02F    PICTURE X.
           02  FILLER REDEFINES TDATE02F.
             03 TDATE02A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TDATE02I  PIC X(8).
           02  TDESC02L    COMP  PIC  S9(4).
           02  TDESC02F    PICTURE X.
           02  FILLER REDEFINES TDESC02F.
             03 TDESC02A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TDESC02I  PIC X(26).
           02  TAMT002L    COMP  PIC  S9(4).
           02  TAMT002F    PICTURE X.
           02  FILLER REDEFINES TAMT002F.
             03 TAMT002A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TAMT002I  PIC X(12).
           02  SEL0003L    COMP  PIC  S9(4).
           02  SEL0003F    PICTURE X.
           02  FILLER REDEFINES SEL0003F.
             03 SEL0003A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  SEL0003I  PIC X(1).
           02  TRNID03L    COMP  PIC  S9(4).
           02  TRNID03F    PICTURE X.
           02  FILLER REDEFINES TRNID03F.
             03 TRNID03A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRNID03I  PIC X(16).
           02  TDATE03L    COMP  PIC  S9(4).
           02  TDATE03F    PICTURE X.
           02  FILLER REDEFINES TDATE03F.
             03 TDATE03A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TDATE03I  PIC X(8).
           02  TDESC03L    COMP  PIC  S9(4).
           02  TDESC03F    PICTURE X.
           02  FILLER REDEFINES TDESC03F.
             03 TDESC03A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TDESC03I  PIC X(26).
           02  TAMT003L    COMP  PIC  S9(4).
           02  TAMT003F    PICTURE X.
           02  FILLER REDEFINES TAMT003F.
             03 TAMT003A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TAMT003I  PIC X(12).
           02  SEL0004L    COMP  PIC  S9(4).
           02  SEL0004F    PICTURE X.
           02  FILLER REDEFINES SEL0004F.
             03 SEL0004A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  SEL0004I  PIC X(1).
           02  TRNID04L    COMP  PIC  S9(4).
           02  TRNID04F    PICTURE X.
           02  FILLER REDEFINES TRNID04F.
             03 TRNID04A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRNID04I  PIC X(16).
           02  TDATE04L    COMP  PIC  S9(4).
           02  TDATE04F    PICTURE X.
           02  FILLER REDEFINES TDATE04F.
             03 TDATE04A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TDATE04I  PIC X(8).
           02  TDESC04L    COMP  PIC  S9(4).
           02  TDESC04F    PICTURE X.
           02  FILLER REDEFINES TDESC04F.
             03 TDESC04A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TDESC04I  PIC X(26).
           02  TAMT004L    COMP  PIC  S9(4).
           02  TAMT004F    PICTURE X.
           02  FILLER REDEFINES TAMT004F.
             03 TAMT004A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TAMT004I  PIC X(12).
           02  SEL0005L    COMP  PIC  S9(4).
           02  SEL0005F    PICTURE X.
           02  FILLER REDEFINES SEL0005F.
             03 SEL0005A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  SEL0005I  PIC X(1).
           02  TRNID05L    COMP  PIC  S9(4).
           02  TRNID05F    PICTURE X.
           02  FILLER REDEFINES TRNID05F.
             03 TRNID05A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRNID05I  PIC X(16).
           02  TDATE05L    COMP  PIC  S9(4).
           02  TDATE05F    PICTURE X.
           02  FILLER REDEFINES TDATE05F.
             03 TDATE05A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TDATE05I  PIC X(8).
           02  TDESC05L    COMP  PIC  S9(4).
           02  TDESC05F    PICTURE X.
           02  FILLER REDEFINES TDESC05F.
             03 TDESC05A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TDESC05I  PIC X(26).
           02  TAMT005L    COMP  PIC  S9(4).
           02  TAMT005F    PICTURE X.
           02  FILLER REDEFINES TAMT005F.
             03 TAMT005A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TAMT005I  PIC X(12).
           02  SEL0006L    COMP  PIC  S9(4).
           02  SEL0006F    PICTURE X.
           02  FILLER REDEFINES SEL0006F.
             03 SEL0006A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  SEL0006I  PIC X(1).
           02  TRNID06L    COMP  PIC  S9(4).
           02  TRNID06F    PICTURE X.
           02  FILLER REDEFINES TRNID06F.
             03 TRNID06A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRNID06I  PIC X(16).
           02  TDATE06L    COMP  PIC  S9(4).
           02  TDATE06F    PICTURE X.
           02  FILLER REDEFINES TDATE06F.
             03 TDATE06A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TDATE06I  PIC X(8).
           02  TDESC06L    COMP  PIC  S9(4).
           02  TDESC06F    PICTURE X.
           02  FILLER REDEFINES TDESC06F.
             03 TDESC06A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TDESC06I  PIC X(26).
           02  TAMT006L    COMP  PIC  S9(4).
           02  TAMT006F    PICTURE X.
           02  FILLER REDEFINES TAMT006F.
             03 TAMT006A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TAMT006I  PIC X(12).
           02  SEL0007L    COMP  PIC  S9(4).
           02  SEL0007F    PICTURE X.
           02  FILLER REDEFINES SEL0007F.
             03 SEL0007A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  SEL0007I  PIC X(1).
           02  TRNID07L    COMP  PIC  S9(4).
           02  TRNID07F    PICTURE X.
           02  FILLER REDEFINES TRNID07F.
             03 TRNID07A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRNID07I  PIC X(16).
           02  TDATE07L    COMP  PIC  S9(4).
           02  TDATE07F    PICTURE X.
           02  FILLER REDEFINES TDATE07F.
             03 TDATE07A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TDATE07I  PIC X(8).
           02  TDESC07L    COMP  PIC  S9(4).
           02  TDESC07F    PICTURE X.
           02  FILLER REDEFINES TDESC07F.
             03 TDESC07A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TDESC07I  PIC X(26).
           02  TAMT007L    COMP  PIC  S9(4).
           02  TAMT007F    PICTURE X.
           02  FILLER REDEFINES TAMT007F.
             03 TAMT007A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TAMT007I  PIC X(12).
           02  SEL0008L    COMP  PIC  S9(4).
           02  SEL0008F    PICTURE X.
           02  FILLER REDEFINES SEL0008F.
             03 SEL0008A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  SEL0008I  PIC X(1).
           02  TRNID08L    COMP  PIC  S9(4).
           02  TRNID08F    PICTURE X.
           02  FILLER REDEFINES TRNID08F.
             03 TRNID08A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRNID08I  PIC X(16).
           02  TDATE08L    COMP  PIC  S9(4).
           02  TDATE08F    PICTURE X.
           02  FILLER REDEFINES TDATE08F.
             03 TDATE08A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TDATE08I  PIC X(8).
           02  TDESC08L    COMP  PIC  S9(4).
           02  TDESC08F    PICTURE X.
           02  FILLER REDEFINES TDESC08F.
             03 TDESC08A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TDESC08I  PIC X(26).
           02  TAMT008L    COMP  PIC  S9(4).
           02  TAMT008F    PICTURE X.
           02  FILLER REDEFINES TAMT008F.
             03 TAMT008A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TAMT008I  PIC X(12).
           02  SEL0009L    COMP  PIC  S9(4).
           02  SEL0009F    PICTURE X.
           02  FILLER REDEFINES SEL0009F.
             03 SEL0009A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  SEL0009I  PIC X(1).
           02  TRNID09L    COMP  PIC  S9(4).
           02  TRNID09F    PICTURE X.
           02  FILLER REDEFINES TRNID09F.
             03 TRNID09A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRNID09I  PIC X(16).
           02  TDATE09L    COMP  PIC  S9(4).
           02  TDATE09F    PICTURE X.
           02  FILLER REDEFINES TDATE09F.
             03 TDATE09A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TDATE09I  PIC X(8).
           02  TDESC09L    COMP  PIC  S9(4).
           02  TDESC09F    PICTURE X.
           02  FILLER REDEFINES TDESC09F.
             03 TDESC09A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TDESC09I  PIC X(26).
           02  TAMT009L    COMP  PIC  S9(4).
           02  TAMT009F    PICTURE X.
           02  FILLER REDEFINES TAMT009F.
             03 TAMT009A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TAMT009I  PIC X(12).
           02  SEL0010L    COMP  PIC  S9(4).
           02  SEL0010F    PICTURE X.
           02  FILLER REDEFINES SEL0010F.
             03 SEL0010A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  SEL0010I  PIC X(1).
           02  TRNID10L    COMP  PIC  S9(4).
           02  TRNID10F    PICTURE X.
           02  FILLER REDEFINES TRNID10F.
             03 TRNID10A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TRNID10I  PIC X(16).
           02  TDATE10L    COMP  PIC  S9(4).
           02  TDATE10F    PICTURE X.
           02  FILLER REDEFINES TDATE10F.
             03 TDATE10A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TDATE10I  PIC X(8).
           02  TDESC10L    COMP  PIC  S9(4).
           02  TDESC10F    PICTURE X.
           02  FILLER REDEFINES TDESC10F.
             03 TDESC10A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TDESC10I  PIC X(26).
           02  TAMT010L    COMP  PIC  S9(4).
           02  TAMT010F    PICTURE X.
           02  FILLER REDEFINES TAMT010F.
             03 TAMT010A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  TAMT010I  PIC X(12).
           02  ERRMSGL    COMP  PIC  S9(4).
           02  ERRMSGF    PICTURE X.
           02  FILLER REDEFINES ERRMSGF.
             03 ERRMSGA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ERRMSGI  PIC X(78).
       01  COTRN0AO REDEFINES COTRN0AI.
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
           02  PAGENUMC    PICTURE X.
           02  PAGENUMP    PICTURE X.
           02  PAGENUMH    PICTURE X.
           02  PAGENUMV    PICTURE X.
           02  PAGENUMO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  TRNIDINC    PICTURE X.
           02  TRNIDINP    PICTURE X.
           02  TRNIDINH    PICTURE X.
           02  TRNIDINV    PICTURE X.
           02  TRNIDINO  PIC X(16).
           02  FILLER PICTURE X(3).
           02  SEL0001C    PICTURE X.
           02  SEL0001P    PICTURE X.
           02  SEL0001H    PICTURE X.
           02  SEL0001V    PICTURE X.
           02  SEL0001O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  TRNID01C    PICTURE X.
           02  TRNID01P    PICTURE X.
           02  TRNID01H    PICTURE X.
           02  TRNID01V    PICTURE X.
           02  TRNID01O  PIC X(16).
           02  FILLER PICTURE X(3).
           02  TDATE01C    PICTURE X.
           02  TDATE01P    PICTURE X.
           02  TDATE01H    PICTURE X.
           02  TDATE01V    PICTURE X.
           02  TDATE01O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  TDESC01C    PICTURE X.
           02  TDESC01P    PICTURE X.
           02  TDESC01H    PICTURE X.
           02  TDESC01V    PICTURE X.
           02  TDESC01O  PIC X(26).
           02  FILLER PICTURE X(3).
           02  TAMT001C    PICTURE X.
           02  TAMT001P    PICTURE X.
           02  TAMT001H    PICTURE X.
           02  TAMT001V    PICTURE X.
           02  TAMT001O  PIC X(12).
           02  FILLER PICTURE X(3).
           02  SEL0002C    PICTURE X.
           02  SEL0002P    PICTURE X.
           02  SEL0002H    PICTURE X.
           02  SEL0002V    PICTURE X.
           02  SEL0002O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  TRNID02C    PICTURE X.
           02  TRNID02P    PICTURE X.
           02  TRNID02H    PICTURE X.
           02  TRNID02V    PICTURE X.
           02  TRNID02O  PIC X(16).
           02  FILLER PICTURE X(3).
           02  TDATE02C    PICTURE X.
           02  TDATE02P    PICTURE X.
           02  TDATE02H    PICTURE X.
           02  TDATE02V    PICTURE X.
           02  TDATE02O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  TDESC02C    PICTURE X.
           02  TDESC02P    PICTURE X.
           02  TDESC02H    PICTURE X.
           02  TDESC02V    PICTURE X.
           02  TDESC02O  PIC X(26).
           02  FILLER PICTURE X(3).
           02  TAMT002C    PICTURE X.
           02  TAMT002P    PICTURE X.
           02  TAMT002H    PICTURE X.
           02  TAMT002V    PICTURE X.
           02  TAMT002O  PIC X(12).
           02  FILLER PICTURE X(3).
           02  SEL0003C    PICTURE X.
           02  SEL0003P    PICTURE X.
           02  SEL0003H    PICTURE X.
           02  SEL0003V    PICTURE X.
           02  SEL0003O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  TRNID03C    PICTURE X.
           02  TRNID03P    PICTURE X.
           02  TRNID03H    PICTURE X.
           02  TRNID03V    PICTURE X.
           02  TRNID03O  PIC X(16).
           02  FILLER PICTURE X(3).
           02  TDATE03C    PICTURE X.
           02  TDATE03P    PICTURE X.
           02  TDATE03H    PICTURE X.
           02  TDATE03V    PICTURE X.
           02  TDATE03O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  TDESC03C    PICTURE X.
           02  TDESC03P    PICTURE X.
           02  TDESC03H    PICTURE X.
           02  TDESC03V    PICTURE X.
           02  TDESC03O  PIC X(26).
           02  FILLER PICTURE X(3).
           02  TAMT003C    PICTURE X.
           02  TAMT003P    PICTURE X.
           02  TAMT003H    PICTURE X.
           02  TAMT003V    PICTURE X.
           02  TAMT003O  PIC X(12).
           02  FILLER PICTURE X(3).
           02  SEL0004C    PICTURE X.
           02  SEL0004P    PICTURE X.
           02  SEL0004H    PICTURE X.
           02  SEL0004V    PICTURE X.
           02  SEL0004O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  TRNID04C    PICTURE X.
           02  TRNID04P    PICTURE X.
           02  TRNID04H    PICTURE X.
           02  TRNID04V    PICTURE X.
           02  TRNID04O  PIC X(16).
           02  FILLER PICTURE X(3).
           02  TDATE04C    PICTURE X.
           02  TDATE04P    PICTURE X.
           02  TDATE04H    PICTURE X.
           02  TDATE04V    PICTURE X.
           02  TDATE04O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  TDESC04C    PICTURE X.
           02  TDESC04P    PICTURE X.
           02  TDESC04H    PICTURE X.
           02  TDESC04V    PICTURE X.
           02  TDESC04O  PIC X(26).
           02  FILLER PICTURE X(3).
           02  TAMT004C    PICTURE X.
           02  TAMT004P    PICTURE X.
           02  TAMT004H    PICTURE X.
           02  TAMT004V    PICTURE X.
           02  TAMT004O  PIC X(12).
           02  FILLER PICTURE X(3).
           02  SEL0005C    PICTURE X.
           02  SEL0005P    PICTURE X.
           02  SEL0005H    PICTURE X.
           02  SEL0005V    PICTURE X.
           02  SEL0005O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  TRNID05C    PICTURE X.
           02  TRNID05P    PICTURE X.
           02  TRNID05H    PICTURE X.
           02  TRNID05V    PICTURE X.
           02  TRNID05O  PIC X(16).
           02  FILLER PICTURE X(3).
           02  TDATE05C    PICTURE X.
           02  TDATE05P    PICTURE X.
           02  TDATE05H    PICTURE X.
           02  TDATE05V    PICTURE X.
           02  TDATE05O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  TDESC05C    PICTURE X.
           02  TDESC05P    PICTURE X.
           02  TDESC05H    PICTURE X.
           02  TDESC05V    PICTURE X.
           02  TDESC05O  PIC X(26).
           02  FILLER PICTURE X(3).
           02  TAMT005C    PICTURE X.
           02  TAMT005P    PICTURE X.
           02  TAMT005H    PICTURE X.
           02  TAMT005V    PICTURE X.
           02  TAMT005O  PIC X(12).
           02  FILLER PICTURE X(3).
           02  SEL0006C    PICTURE X.
           02  SEL0006P    PICTURE X.
           02  SEL0006H    PICTURE X.
           02  SEL0006V    PICTURE X.
           02  SEL0006O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  TRNID06C    PICTURE X.
           02  TRNID06P    PICTURE X.
           02  TRNID06H    PICTURE X.
           02  TRNID06V    PICTURE X.
           02  TRNID06O  PIC X(16).
           02  FILLER PICTURE X(3).
           02  TDATE06C    PICTURE X.
           02  TDATE06P    PICTURE X.
           02  TDATE06H    PICTURE X.
           02  TDATE06V    PICTURE X.
           02  TDATE06O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  TDESC06C    PICTURE X.
           02  TDESC06P    PICTURE X.
           02  TDESC06H    PICTURE X.
           02  TDESC06V    PICTURE X.
           02  TDESC06O  PIC X(26).
           02  FILLER PICTURE X(3).
           02  TAMT006C    PICTURE X.
           02  TAMT006P    PICTURE X.
           02  TAMT006H    PICTURE X.
           02  TAMT006V    PICTURE X.
           02  TAMT006O  PIC X(12).
           02  FILLER PICTURE X(3).
           02  SEL0007C    PICTURE X.
           02  SEL0007P    PICTURE X.
           02  SEL0007H    PICTURE X.
           02  SEL0007V    PICTURE X.
           02  SEL0007O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  TRNID07C    PICTURE X.
           02  TRNID07P    PICTURE X.
           02  TRNID07H    PICTURE X.
           02  TRNID07V    PICTURE X.
           02  TRNID07O  PIC X(16).
           02  FILLER PICTURE X(3).
           02  TDATE07C    PICTURE X.
           02  TDATE07P    PICTURE X.
           02  TDATE07H    PICTURE X.
           02  TDATE07V    PICTURE X.
           02  TDATE07O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  TDESC07C    PICTURE X.
           02  TDESC07P    PICTURE X.
           02  TDESC07H    PICTURE X.
           02  TDESC07V    PICTURE X.
           02  TDESC07O  PIC X(26).
           02  FILLER PICTURE X(3).
           02  TAMT007C    PICTURE X.
           02  TAMT007P    PICTURE X.
           02  TAMT007H    PICTURE X.
           02  TAMT007V    PICTURE X.
           02  TAMT007O  PIC X(12).
           02  FILLER PICTURE X(3).
           02  SEL0008C    PICTURE X.
           02  SEL0008P    PICTURE X.
           02  SEL0008H    PICTURE X.
           02  SEL0008V    PICTURE X.
           02  SEL0008O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  TRNID08C    PICTURE X.
           02  TRNID08P    PICTURE X.
           02  TRNID08H    PICTURE X.
           02  TRNID08V    PICTURE X.
           02  TRNID08O  PIC X(16).
           02  FILLER PICTURE X(3).
           02  TDATE08C    PICTURE X.
           02  TDATE08P    PICTURE X.
           02  TDATE08H    PICTURE X.
           02  TDATE08V    PICTURE X.
           02  TDATE08O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  TDESC08C    PICTURE X.
           02  TDESC08P    PICTURE X.
           02  TDESC08H    PICTURE X.
           02  TDESC08V    PICTURE X.
           02  TDESC08O  PIC X(26).
           02  FILLER PICTURE X(3).
           02  TAMT008C    PICTURE X.
           02  TAMT008P    PICTURE X.
           02  TAMT008H    PICTURE X.
           02  TAMT008V    PICTURE X.
           02  TAMT008O  PIC X(12).
           02  FILLER PICTURE X(3).
           02  SEL0009C    PICTURE X.
           02  SEL0009P    PICTURE X.
           02  SEL0009H    PICTURE X.
           02  SEL0009V    PICTURE X.
           02  SEL0009O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  TRNID09C    PICTURE X.
           02  TRNID09P    PICTURE X.
           02  TRNID09H    PICTURE X.
           02  TRNID09V    PICTURE X.
           02  TRNID09O  PIC X(16).
           02  FILLER PICTURE X(3).
           02  TDATE09C    PICTURE X.
           02  TDATE09P    PICTURE X.
           02  TDATE09H    PICTURE X.
           02  TDATE09V    PICTURE X.
           02  TDATE09O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  TDESC09C    PICTURE X.
           02  TDESC09P    PICTURE X.
           02  TDESC09H    PICTURE X.
           02  TDESC09V    PICTURE X.
           02  TDESC09O  PIC X(26).
           02  FILLER PICTURE X(3).
           02  TAMT009C    PICTURE X.
           02  TAMT009P    PICTURE X.
           02  TAMT009H    PICTURE X.
           02  TAMT009V    PICTURE X.
           02  TAMT009O  PIC X(12).
           02  FILLER PICTURE X(3).
           02  SEL0010C    PICTURE X.
           02  SEL0010P    PICTURE X.
           02  SEL0010H    PICTURE X.
           02  SEL0010V    PICTURE X.
           02  SEL0010O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  TRNID10C    PICTURE X.
           02  TRNID10P    PICTURE X.
           02  TRNID10H    PICTURE X.
           02  TRNID10V    PICTURE X.
           02  TRNID10O  PIC X(16).
           02  FILLER PICTURE X(3).
           02  TDATE10C    PICTURE X.
           02  TDATE10P    PICTURE X.
           02  TDATE10H    PICTURE X.
           02  TDATE10V    PICTURE X.
           02  TDATE10O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  TDESC10C    PICTURE X.
           02  TDESC10P    PICTURE X.
           02  TDESC10H    PICTURE X.
           02  TDESC10V    PICTURE X.
           02  TDESC10O  PIC X(26).
           02  FILLER PICTURE X(3).
           02  TAMT010C    PICTURE X.
           02  TAMT010P    PICTURE X.
           02  TAMT010H    PICTURE X.
           02  TAMT010V    PICTURE X.
           02  TAMT010O  PIC X(12).
           02  FILLER PICTURE X(3).
           02  ERRMSGC    PICTURE X.
           02  ERRMSGP    PICTURE X.
           02  ERRMSGH    PICTURE X.
           02  ERRMSGV    PICTURE X.
           02  ERRMSGO  PIC X(78).
