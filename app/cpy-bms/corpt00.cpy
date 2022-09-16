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
       01  CORPT0AI.
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
           02  MONTHLYL    COMP  PIC  S9(4).
           02  MONTHLYF    PICTURE X.
           02  FILLER REDEFINES MONTHLYF.
             03 MONTHLYA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  MONTHLYI  PIC X(1).
           02  YEARLYL    COMP  PIC  S9(4).
           02  YEARLYF    PICTURE X.
           02  FILLER REDEFINES YEARLYF.
             03 YEARLYA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  YEARLYI  PIC X(1).
           02  CUSTOML    COMP  PIC  S9(4).
           02  CUSTOMF    PICTURE X.
           02  FILLER REDEFINES CUSTOMF.
             03 CUSTOMA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CUSTOMI  PIC X(1).
           02  SDTMML    COMP  PIC  S9(4).
           02  SDTMMF    PICTURE X.
           02  FILLER REDEFINES SDTMMF.
             03 SDTMMA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  SDTMMI  PIC X(2).
           02  SDTDDL    COMP  PIC  S9(4).
           02  SDTDDF    PICTURE X.
           02  FILLER REDEFINES SDTDDF.
             03 SDTDDA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  SDTDDI  PIC X(2).
           02  SDTYYYYL    COMP  PIC  S9(4).
           02  SDTYYYYF    PICTURE X.
           02  FILLER REDEFINES SDTYYYYF.
             03 SDTYYYYA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  SDTYYYYI  PIC X(4).
           02  EDTMML    COMP  PIC  S9(4).
           02  EDTMMF    PICTURE X.
           02  FILLER REDEFINES EDTMMF.
             03 EDTMMA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  EDTMMI  PIC X(2).
           02  EDTDDL    COMP  PIC  S9(4).
           02  EDTDDF    PICTURE X.
           02  FILLER REDEFINES EDTDDF.
             03 EDTDDA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  EDTDDI  PIC X(2).
           02  EDTYYYYL    COMP  PIC  S9(4).
           02  EDTYYYYF    PICTURE X.
           02  FILLER REDEFINES EDTYYYYF.
             03 EDTYYYYA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  EDTYYYYI  PIC X(4).
           02  CONFIRML    COMP  PIC  S9(4).
           02  CONFIRMF    PICTURE X.
           02  FILLER REDEFINES CONFIRMF.
             03 CONFIRMA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CONFIRMI  PIC X(1).
           02  ERRMSGL    COMP  PIC  S9(4).
           02  ERRMSGF    PICTURE X.
           02  FILLER REDEFINES ERRMSGF.
             03 ERRMSGA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ERRMSGI  PIC X(78).
       01  CORPT0AO REDEFINES CORPT0AI.
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
           02  MONTHLYC    PICTURE X.
           02  MONTHLYP    PICTURE X.
           02  MONTHLYH    PICTURE X.
           02  MONTHLYV    PICTURE X.
           02  MONTHLYO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  YEARLYC    PICTURE X.
           02  YEARLYP    PICTURE X.
           02  YEARLYH    PICTURE X.
           02  YEARLYV    PICTURE X.
           02  YEARLYO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  CUSTOMC    PICTURE X.
           02  CUSTOMP    PICTURE X.
           02  CUSTOMH    PICTURE X.
           02  CUSTOMV    PICTURE X.
           02  CUSTOMO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  SDTMMC    PICTURE X.
           02  SDTMMP    PICTURE X.
           02  SDTMMH    PICTURE X.
           02  SDTMMV    PICTURE X.
           02  SDTMMO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  SDTDDC    PICTURE X.
           02  SDTDDP    PICTURE X.
           02  SDTDDH    PICTURE X.
           02  SDTDDV    PICTURE X.
           02  SDTDDO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  SDTYYYYC    PICTURE X.
           02  SDTYYYYP    PICTURE X.
           02  SDTYYYYH    PICTURE X.
           02  SDTYYYYV    PICTURE X.
           02  SDTYYYYO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  EDTMMC    PICTURE X.
           02  EDTMMP    PICTURE X.
           02  EDTMMH    PICTURE X.
           02  EDTMMV    PICTURE X.
           02  EDTMMO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  EDTDDC    PICTURE X.
           02  EDTDDP    PICTURE X.
           02  EDTDDH    PICTURE X.
           02  EDTDDV    PICTURE X.
           02  EDTDDO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  EDTYYYYC    PICTURE X.
           02  EDTYYYYP    PICTURE X.
           02  EDTYYYYH    PICTURE X.
           02  EDTYYYYV    PICTURE X.
           02  EDTYYYYO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  CONFIRMC    PICTURE X.
           02  CONFIRMP    PICTURE X.
           02  CONFIRMH    PICTURE X.
           02  CONFIRMV    PICTURE X.
           02  CONFIRMO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ERRMSGC    PICTURE X.
           02  ERRMSGP    PICTURE X.
           02  ERRMSGH    PICTURE X.
           02  ERRMSGV    PICTURE X.
           02  ERRMSGO  PIC X(78).
