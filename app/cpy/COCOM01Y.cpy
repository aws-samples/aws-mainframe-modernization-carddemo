      ******************************************************************
      * Communication area for CardDemo application programs
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
       01 CARDDEMO-COMMAREA.
          05 CDEMO-GENERAL-INFO.
             10 CDEMO-FROM-TRANID             PIC X(04).
             10 CDEMO-FROM-PROGRAM            PIC X(08).
             10 CDEMO-TO-TRANID               PIC X(04).
             10 CDEMO-TO-PROGRAM              PIC X(08).
             10 CDEMO-USER-ID                 PIC X(08).
             10 CDEMO-USER-TYPE               PIC X(01).
                88 CDEMO-USRTYP-ADMIN         VALUE 'A'.
                88 CDEMO-USRTYP-USER          VALUE 'U'.
             10 CDEMO-PGM-CONTEXT             PIC 9(01).
                88 CDEMO-PGM-ENTER            VALUE 0.
                88 CDEMO-PGM-REENTER          VALUE 1.
          05 CDEMO-CUSTOMER-INFO.
             10 CDEMO-CUST-ID                 PIC 9(09).
             10 CDEMO-CUST-FNAME              PIC X(25).
             10 CDEMO-CUST-MNAME              PIC X(25).
             10 CDEMO-CUST-LNAME              PIC X(25).
          05 CDEMO-ACCOUNT-INFO.
             10 CDEMO-ACCT-ID                 PIC 9(11).
             10 CDEMO-ACCT-STATUS             PIC X(01).
          05 CDEMO-CARD-INFO.
             10 CDEMO-CARD-NUM                PIC 9(16).
          05 CDEMO-MORE-INFO.
             10  CDEMO-LAST-MAP               PIC X(7).
             10  CDEMO-LAST-MAPSET            PIC X(7).
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:15:57 CDT
      *
