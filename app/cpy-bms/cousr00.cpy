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
       01  COUSR0AI.
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
           02  USRIDINL    COMP  PIC  S9(4).
           02  USRIDINF    PICTURE X.
           02  FILLER REDEFINES USRIDINF.
             03 USRIDINA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  USRIDINI  PIC X(8).
           02  SEL0001L    COMP  PIC  S9(4).
           02  SEL0001F    PICTURE X.
           02  FILLER REDEFINES SEL0001F.
             03 SEL0001A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  SEL0001I  PIC X(1).
           02  USRID01L    COMP  PIC  S9(4).
           02  USRID01F    PICTURE X.
           02  FILLER REDEFINES USRID01F.
             03 USRID01A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  USRID01I  PIC X(8).
           02  FNAME01L    COMP  PIC  S9(4).
           02  FNAME01F    PICTURE X.
           02  FILLER REDEFINES FNAME01F.
             03 FNAME01A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  FNAME01I  PIC X(20).
           02  LNAME01L    COMP  PIC  S9(4).
           02  LNAME01F    PICTURE X.
           02  FILLER REDEFINES LNAME01F.
             03 LNAME01A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  LNAME01I  PIC X(20).
           02  UTYPE01L    COMP  PIC  S9(4).
           02  UTYPE01F    PICTURE X.
           02  FILLER REDEFINES UTYPE01F.
             03 UTYPE01A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  UTYPE01I  PIC X(1).
           02  SEL0002L    COMP  PIC  S9(4).
           02  SEL0002F    PICTURE X.
           02  FILLER REDEFINES SEL0002F.
             03 SEL0002A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  SEL0002I  PIC X(1).
           02  USRID02L    COMP  PIC  S9(4).
           02  USRID02F    PICTURE X.
           02  FILLER REDEFINES USRID02F.
             03 USRID02A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  USRID02I  PIC X(8).
           02  FNAME02L    COMP  PIC  S9(4).
           02  FNAME02F    PICTURE X.
           02  FILLER REDEFINES FNAME02F.
             03 FNAME02A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  FNAME02I  PIC X(20).
           02  LNAME02L    COMP  PIC  S9(4).
           02  LNAME02F    PICTURE X.
           02  FILLER REDEFINES LNAME02F.
             03 LNAME02A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  LNAME02I  PIC X(20).
           02  UTYPE02L    COMP  PIC  S9(4).
           02  UTYPE02F    PICTURE X.
           02  FILLER REDEFINES UTYPE02F.
             03 UTYPE02A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  UTYPE02I  PIC X(1).
           02  SEL0003L    COMP  PIC  S9(4).
           02  SEL0003F    PICTURE X.
           02  FILLER REDEFINES SEL0003F.
             03 SEL0003A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  SEL0003I  PIC X(1).
           02  USRID03L    COMP  PIC  S9(4).
           02  USRID03F    PICTURE X.
           02  FILLER REDEFINES USRID03F.
             03 USRID03A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  USRID03I  PIC X(8).
           02  FNAME03L    COMP  PIC  S9(4).
           02  FNAME03F    PICTURE X.
           02  FILLER REDEFINES FNAME03F.
             03 FNAME03A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  FNAME03I  PIC X(20).
           02  LNAME03L    COMP  PIC  S9(4).
           02  LNAME03F    PICTURE X.
           02  FILLER REDEFINES LNAME03F.
             03 LNAME03A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  LNAME03I  PIC X(20).
           02  UTYPE03L    COMP  PIC  S9(4).
           02  UTYPE03F    PICTURE X.
           02  FILLER REDEFINES UTYPE03F.
             03 UTYPE03A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  UTYPE03I  PIC X(1).
           02  SEL0004L    COMP  PIC  S9(4).
           02  SEL0004F    PICTURE X.
           02  FILLER REDEFINES SEL0004F.
             03 SEL0004A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  SEL0004I  PIC X(1).
           02  USRID04L    COMP  PIC  S9(4).
           02  USRID04F    PICTURE X.
           02  FILLER REDEFINES USRID04F.
             03 USRID04A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  USRID04I  PIC X(8).
           02  FNAME04L    COMP  PIC  S9(4).
           02  FNAME04F    PICTURE X.
           02  FILLER REDEFINES FNAME04F.
             03 FNAME04A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  FNAME04I  PIC X(20).
           02  LNAME04L    COMP  PIC  S9(4).
           02  LNAME04F    PICTURE X.
           02  FILLER REDEFINES LNAME04F.
             03 LNAME04A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  LNAME04I  PIC X(20).
           02  UTYPE04L    COMP  PIC  S9(4).
           02  UTYPE04F    PICTURE X.
           02  FILLER REDEFINES UTYPE04F.
             03 UTYPE04A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  UTYPE04I  PIC X(1).
           02  SEL0005L    COMP  PIC  S9(4).
           02  SEL0005F    PICTURE X.
           02  FILLER REDEFINES SEL0005F.
             03 SEL0005A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  SEL0005I  PIC X(1).
           02  USRID05L    COMP  PIC  S9(4).
           02  USRID05F    PICTURE X.
           02  FILLER REDEFINES USRID05F.
             03 USRID05A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  USRID05I  PIC X(8).
           02  FNAME05L    COMP  PIC  S9(4).
           02  FNAME05F    PICTURE X.
           02  FILLER REDEFINES FNAME05F.
             03 FNAME05A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  FNAME05I  PIC X(20).
           02  LNAME05L    COMP  PIC  S9(4).
           02  LNAME05F    PICTURE X.
           02  FILLER REDEFINES LNAME05F.
             03 LNAME05A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  LNAME05I  PIC X(20).
           02  UTYPE05L    COMP  PIC  S9(4).
           02  UTYPE05F    PICTURE X.
           02  FILLER REDEFINES UTYPE05F.
             03 UTYPE05A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  UTYPE05I  PIC X(1).
           02  SEL0006L    COMP  PIC  S9(4).
           02  SEL0006F    PICTURE X.
           02  FILLER REDEFINES SEL0006F.
             03 SEL0006A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  SEL0006I  PIC X(1).
           02  USRID06L    COMP  PIC  S9(4).
           02  USRID06F    PICTURE X.
           02  FILLER REDEFINES USRID06F.
             03 USRID06A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  USRID06I  PIC X(8).
           02  FNAME06L    COMP  PIC  S9(4).
           02  FNAME06F    PICTURE X.
           02  FILLER REDEFINES FNAME06F.
             03 FNAME06A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  FNAME06I  PIC X(20).
           02  LNAME06L    COMP  PIC  S9(4).
           02  LNAME06F    PICTURE X.
           02  FILLER REDEFINES LNAME06F.
             03 LNAME06A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  LNAME06I  PIC X(20).
           02  UTYPE06L    COMP  PIC  S9(4).
           02  UTYPE06F    PICTURE X.
           02  FILLER REDEFINES UTYPE06F.
             03 UTYPE06A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  UTYPE06I  PIC X(1).
           02  SEL0007L    COMP  PIC  S9(4).
           02  SEL0007F    PICTURE X.
           02  FILLER REDEFINES SEL0007F.
             03 SEL0007A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  SEL0007I  PIC X(1).
           02  USRID07L    COMP  PIC  S9(4).
           02  USRID07F    PICTURE X.
           02  FILLER REDEFINES USRID07F.
             03 USRID07A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  USRID07I  PIC X(8).
           02  FNAME07L    COMP  PIC  S9(4).
           02  FNAME07F    PICTURE X.
           02  FILLER REDEFINES FNAME07F.
             03 FNAME07A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  FNAME07I  PIC X(20).
           02  LNAME07L    COMP  PIC  S9(4).
           02  LNAME07F    PICTURE X.
           02  FILLER REDEFINES LNAME07F.
             03 LNAME07A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  LNAME07I  PIC X(20).
           02  UTYPE07L    COMP  PIC  S9(4).
           02  UTYPE07F    PICTURE X.
           02  FILLER REDEFINES UTYPE07F.
             03 UTYPE07A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  UTYPE07I  PIC X(1).
           02  SEL0008L    COMP  PIC  S9(4).
           02  SEL0008F    PICTURE X.
           02  FILLER REDEFINES SEL0008F.
             03 SEL0008A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  SEL0008I  PIC X(1).
           02  USRID08L    COMP  PIC  S9(4).
           02  USRID08F    PICTURE X.
           02  FILLER REDEFINES USRID08F.
             03 USRID08A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  USRID08I  PIC X(8).
           02  FNAME08L    COMP  PIC  S9(4).
           02  FNAME08F    PICTURE X.
           02  FILLER REDEFINES FNAME08F.
             03 FNAME08A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  FNAME08I  PIC X(20).
           02  LNAME08L    COMP  PIC  S9(4).
           02  LNAME08F    PICTURE X.
           02  FILLER REDEFINES LNAME08F.
             03 LNAME08A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  LNAME08I  PIC X(20).
           02  UTYPE08L    COMP  PIC  S9(4).
           02  UTYPE08F    PICTURE X.
           02  FILLER REDEFINES UTYPE08F.
             03 UTYPE08A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  UTYPE08I  PIC X(1).
           02  SEL0009L    COMP  PIC  S9(4).
           02  SEL0009F    PICTURE X.
           02  FILLER REDEFINES SEL0009F.
             03 SEL0009A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  SEL0009I  PIC X(1).
           02  USRID09L    COMP  PIC  S9(4).
           02  USRID09F    PICTURE X.
           02  FILLER REDEFINES USRID09F.
             03 USRID09A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  USRID09I  PIC X(8).
           02  FNAME09L    COMP  PIC  S9(4).
           02  FNAME09F    PICTURE X.
           02  FILLER REDEFINES FNAME09F.
             03 FNAME09A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  FNAME09I  PIC X(20).
           02  LNAME09L    COMP  PIC  S9(4).
           02  LNAME09F    PICTURE X.
           02  FILLER REDEFINES LNAME09F.
             03 LNAME09A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  LNAME09I  PIC X(20).
           02  UTYPE09L    COMP  PIC  S9(4).
           02  UTYPE09F    PICTURE X.
           02  FILLER REDEFINES UTYPE09F.
             03 UTYPE09A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  UTYPE09I  PIC X(1).
           02  SEL0010L    COMP  PIC  S9(4).
           02  SEL0010F    PICTURE X.
           02  FILLER REDEFINES SEL0010F.
             03 SEL0010A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  SEL0010I  PIC X(1).
           02  USRID10L    COMP  PIC  S9(4).
           02  USRID10F    PICTURE X.
           02  FILLER REDEFINES USRID10F.
             03 USRID10A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  USRID10I  PIC X(8).
           02  FNAME10L    COMP  PIC  S9(4).
           02  FNAME10F    PICTURE X.
           02  FILLER REDEFINES FNAME10F.
             03 FNAME10A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  FNAME10I  PIC X(20).
           02  LNAME10L    COMP  PIC  S9(4).
           02  LNAME10F    PICTURE X.
           02  FILLER REDEFINES LNAME10F.
             03 LNAME10A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  LNAME10I  PIC X(20).
           02  UTYPE10L    COMP  PIC  S9(4).
           02  UTYPE10F    PICTURE X.
           02  FILLER REDEFINES UTYPE10F.
             03 UTYPE10A    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  UTYPE10I  PIC X(1).
           02  ERRMSGL    COMP  PIC  S9(4).
           02  ERRMSGF    PICTURE X.
           02  FILLER REDEFINES ERRMSGF.
             03 ERRMSGA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ERRMSGI  PIC X(78).
       01  COUSR0AO REDEFINES COUSR0AI.
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
           02  USRIDINC    PICTURE X.
           02  USRIDINP    PICTURE X.
           02  USRIDINH    PICTURE X.
           02  USRIDINV    PICTURE X.
           02  USRIDINO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  SEL0001C    PICTURE X.
           02  SEL0001P    PICTURE X.
           02  SEL0001H    PICTURE X.
           02  SEL0001V    PICTURE X.
           02  SEL0001O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  USRID01C    PICTURE X.
           02  USRID01P    PICTURE X.
           02  USRID01H    PICTURE X.
           02  USRID01V    PICTURE X.
           02  USRID01O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  FNAME01C    PICTURE X.
           02  FNAME01P    PICTURE X.
           02  FNAME01H    PICTURE X.
           02  FNAME01V    PICTURE X.
           02  FNAME01O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  LNAME01C    PICTURE X.
           02  LNAME01P    PICTURE X.
           02  LNAME01H    PICTURE X.
           02  LNAME01V    PICTURE X.
           02  LNAME01O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  UTYPE01C    PICTURE X.
           02  UTYPE01P    PICTURE X.
           02  UTYPE01H    PICTURE X.
           02  UTYPE01V    PICTURE X.
           02  UTYPE01O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  SEL0002C    PICTURE X.
           02  SEL0002P    PICTURE X.
           02  SEL0002H    PICTURE X.
           02  SEL0002V    PICTURE X.
           02  SEL0002O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  USRID02C    PICTURE X.
           02  USRID02P    PICTURE X.
           02  USRID02H    PICTURE X.
           02  USRID02V    PICTURE X.
           02  USRID02O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  FNAME02C    PICTURE X.
           02  FNAME02P    PICTURE X.
           02  FNAME02H    PICTURE X.
           02  FNAME02V    PICTURE X.
           02  FNAME02O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  LNAME02C    PICTURE X.
           02  LNAME02P    PICTURE X.
           02  LNAME02H    PICTURE X.
           02  LNAME02V    PICTURE X.
           02  LNAME02O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  UTYPE02C    PICTURE X.
           02  UTYPE02P    PICTURE X.
           02  UTYPE02H    PICTURE X.
           02  UTYPE02V    PICTURE X.
           02  UTYPE02O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  SEL0003C    PICTURE X.
           02  SEL0003P    PICTURE X.
           02  SEL0003H    PICTURE X.
           02  SEL0003V    PICTURE X.
           02  SEL0003O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  USRID03C    PICTURE X.
           02  USRID03P    PICTURE X.
           02  USRID03H    PICTURE X.
           02  USRID03V    PICTURE X.
           02  USRID03O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  FNAME03C    PICTURE X.
           02  FNAME03P    PICTURE X.
           02  FNAME03H    PICTURE X.
           02  FNAME03V    PICTURE X.
           02  FNAME03O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  LNAME03C    PICTURE X.
           02  LNAME03P    PICTURE X.
           02  LNAME03H    PICTURE X.
           02  LNAME03V    PICTURE X.
           02  LNAME03O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  UTYPE03C    PICTURE X.
           02  UTYPE03P    PICTURE X.
           02  UTYPE03H    PICTURE X.
           02  UTYPE03V    PICTURE X.
           02  UTYPE03O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  SEL0004C    PICTURE X.
           02  SEL0004P    PICTURE X.
           02  SEL0004H    PICTURE X.
           02  SEL0004V    PICTURE X.
           02  SEL0004O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  USRID04C    PICTURE X.
           02  USRID04P    PICTURE X.
           02  USRID04H    PICTURE X.
           02  USRID04V    PICTURE X.
           02  USRID04O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  FNAME04C    PICTURE X.
           02  FNAME04P    PICTURE X.
           02  FNAME04H    PICTURE X.
           02  FNAME04V    PICTURE X.
           02  FNAME04O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  LNAME04C    PICTURE X.
           02  LNAME04P    PICTURE X.
           02  LNAME04H    PICTURE X.
           02  LNAME04V    PICTURE X.
           02  LNAME04O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  UTYPE04C    PICTURE X.
           02  UTYPE04P    PICTURE X.
           02  UTYPE04H    PICTURE X.
           02  UTYPE04V    PICTURE X.
           02  UTYPE04O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  SEL0005C    PICTURE X.
           02  SEL0005P    PICTURE X.
           02  SEL0005H    PICTURE X.
           02  SEL0005V    PICTURE X.
           02  SEL0005O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  USRID05C    PICTURE X.
           02  USRID05P    PICTURE X.
           02  USRID05H    PICTURE X.
           02  USRID05V    PICTURE X.
           02  USRID05O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  FNAME05C    PICTURE X.
           02  FNAME05P    PICTURE X.
           02  FNAME05H    PICTURE X.
           02  FNAME05V    PICTURE X.
           02  FNAME05O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  LNAME05C    PICTURE X.
           02  LNAME05P    PICTURE X.
           02  LNAME05H    PICTURE X.
           02  LNAME05V    PICTURE X.
           02  LNAME05O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  UTYPE05C    PICTURE X.
           02  UTYPE05P    PICTURE X.
           02  UTYPE05H    PICTURE X.
           02  UTYPE05V    PICTURE X.
           02  UTYPE05O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  SEL0006C    PICTURE X.
           02  SEL0006P    PICTURE X.
           02  SEL0006H    PICTURE X.
           02  SEL0006V    PICTURE X.
           02  SEL0006O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  USRID06C    PICTURE X.
           02  USRID06P    PICTURE X.
           02  USRID06H    PICTURE X.
           02  USRID06V    PICTURE X.
           02  USRID06O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  FNAME06C    PICTURE X.
           02  FNAME06P    PICTURE X.
           02  FNAME06H    PICTURE X.
           02  FNAME06V    PICTURE X.
           02  FNAME06O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  LNAME06C    PICTURE X.
           02  LNAME06P    PICTURE X.
           02  LNAME06H    PICTURE X.
           02  LNAME06V    PICTURE X.
           02  LNAME06O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  UTYPE06C    PICTURE X.
           02  UTYPE06P    PICTURE X.
           02  UTYPE06H    PICTURE X.
           02  UTYPE06V    PICTURE X.
           02  UTYPE06O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  SEL0007C    PICTURE X.
           02  SEL0007P    PICTURE X.
           02  SEL0007H    PICTURE X.
           02  SEL0007V    PICTURE X.
           02  SEL0007O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  USRID07C    PICTURE X.
           02  USRID07P    PICTURE X.
           02  USRID07H    PICTURE X.
           02  USRID07V    PICTURE X.
           02  USRID07O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  FNAME07C    PICTURE X.
           02  FNAME07P    PICTURE X.
           02  FNAME07H    PICTURE X.
           02  FNAME07V    PICTURE X.
           02  FNAME07O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  LNAME07C    PICTURE X.
           02  LNAME07P    PICTURE X.
           02  LNAME07H    PICTURE X.
           02  LNAME07V    PICTURE X.
           02  LNAME07O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  UTYPE07C    PICTURE X.
           02  UTYPE07P    PICTURE X.
           02  UTYPE07H    PICTURE X.
           02  UTYPE07V    PICTURE X.
           02  UTYPE07O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  SEL0008C    PICTURE X.
           02  SEL0008P    PICTURE X.
           02  SEL0008H    PICTURE X.
           02  SEL0008V    PICTURE X.
           02  SEL0008O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  USRID08C    PICTURE X.
           02  USRID08P    PICTURE X.
           02  USRID08H    PICTURE X.
           02  USRID08V    PICTURE X.
           02  USRID08O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  FNAME08C    PICTURE X.
           02  FNAME08P    PICTURE X.
           02  FNAME08H    PICTURE X.
           02  FNAME08V    PICTURE X.
           02  FNAME08O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  LNAME08C    PICTURE X.
           02  LNAME08P    PICTURE X.
           02  LNAME08H    PICTURE X.
           02  LNAME08V    PICTURE X.
           02  LNAME08O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  UTYPE08C    PICTURE X.
           02  UTYPE08P    PICTURE X.
           02  UTYPE08H    PICTURE X.
           02  UTYPE08V    PICTURE X.
           02  UTYPE08O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  SEL0009C    PICTURE X.
           02  SEL0009P    PICTURE X.
           02  SEL0009H    PICTURE X.
           02  SEL0009V    PICTURE X.
           02  SEL0009O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  USRID09C    PICTURE X.
           02  USRID09P    PICTURE X.
           02  USRID09H    PICTURE X.
           02  USRID09V    PICTURE X.
           02  USRID09O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  FNAME09C    PICTURE X.
           02  FNAME09P    PICTURE X.
           02  FNAME09H    PICTURE X.
           02  FNAME09V    PICTURE X.
           02  FNAME09O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  LNAME09C    PICTURE X.
           02  LNAME09P    PICTURE X.
           02  LNAME09H    PICTURE X.
           02  LNAME09V    PICTURE X.
           02  LNAME09O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  UTYPE09C    PICTURE X.
           02  UTYPE09P    PICTURE X.
           02  UTYPE09H    PICTURE X.
           02  UTYPE09V    PICTURE X.
           02  UTYPE09O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  SEL0010C    PICTURE X.
           02  SEL0010P    PICTURE X.
           02  SEL0010H    PICTURE X.
           02  SEL0010V    PICTURE X.
           02  SEL0010O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  USRID10C    PICTURE X.
           02  USRID10P    PICTURE X.
           02  USRID10H    PICTURE X.
           02  USRID10V    PICTURE X.
           02  USRID10O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  FNAME10C    PICTURE X.
           02  FNAME10P    PICTURE X.
           02  FNAME10H    PICTURE X.
           02  FNAME10V    PICTURE X.
           02  FNAME10O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  LNAME10C    PICTURE X.
           02  LNAME10P    PICTURE X.
           02  LNAME10H    PICTURE X.
           02  LNAME10V    PICTURE X.
           02  LNAME10O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  UTYPE10C    PICTURE X.
           02  UTYPE10P    PICTURE X.
           02  UTYPE10H    PICTURE X.
           02  UTYPE10V    PICTURE X.
           02  UTYPE10O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ERRMSGC    PICTURE X.
           02  ERRMSGP    PICTURE X.
           02  ERRMSGH    PICTURE X.
           02  ERRMSGV    PICTURE X.
           02  ERRMSGO  PIC X(78).
